;;; magma-interactive.el --- Interaction with an external magma process. ;

;; Copyright (C) 2007-2014 Luk Bettale
;;               2013-2014 Thibaut Verron
;; Licensed under the GNU General Public License.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;;; Commentary:

;; Documentation available in README.org or on
;; https://github.com/ThibautVerron/magma-mode

;;; Code:


(require 'comint)
(require 'term)
(require 'compile)

(require 'magma-vars)
(require 'magma-completion)
(require 'magma-q)

(declare-function magma-mode "magma-mode.el")

(declare-function magma-run "magma-interactive.el" t t)
(declare-function magma-interactive-mode "magma-interactive.el" t t)
(declare-function magma-send "magma-interactive.el" t t)
(declare-function magma-help-word-text "magma-interactive.el" t t)


(defcustom magma-interactive-program "magma"
  "Program to be launched to use magma (usually magma)."
  :group 'magma
  :type 'string)

(defcustom magma-interactive-arguments '()
  "Commandline arguments to pass to magma."
  :group 'magma
  :type 'sexp)

(defvar magma-working-buffer-number 0
  "Should this buffer send instructions to a different magma buffer.")

(defun magma--valid-working-buffer-number (num)
  "Is NUM a valid working buffer number?

If this predicate is satisfied, the value  is safe as file-local."
  (or (integerp num)
      (char-or-string-p num)))

(put 'magma-working-buffer-number
     'safe-local-variable
     #'magma--valid-working-buffer-number)

(defvar magma-active-buffers-list '()
  "List of active magma buffers.")

(defvar-local magma-pending-input (magma-q-create)
  "List of strings to be sent to the magma process.

Do not modify this variable directly, the consequences could be
unprevisible. This variable can be useful for use in a
user-function sending input to a process.")

(defvar-local magma-ready t
  "Whether there is still input or output pending in that buffer.")

(defvar-local magma-timer (current-time))


(put 'magma-ready 'permanent-local t)
(put 'magma-pending-input 'permanent-local t)
(put 'magma-timer 'permanent-local t)

(defcustom magma-interactive-buffer-name "magma"
  "Name of the buffer to be used for using magma interactively.

\(will be surrounded by `*')"
  :group 'magma
  :type 'string)

(defun magma-set-skip (symbol value)
  (set-default symbol value)
  (when (and magma-interactive-skip-comments
             (not magma-interactive-skip-empty-lines))
    (warn "magma-interactive-skip-empty-lines is nil, magma-interactive-skip-comments is t. Expect lots of empty lines replacing the comments.")))

(defcustom magma-interactive-skip-empty-lines nil
  "If non-nil, strip empty lines before sending input to the magma process.

  This variable can be set to nil even if
  `magma-interactive-skip-comments' is set to t. However,
  this will probably lead to unwanted behavior, since the
  commented lines are replaced with blank lines."
  :group 'magma
  ;; :set 'magma-set-skip ;; FIXME: find a way to uncomment this without error
  :type 'boolean)

(defcustom magma-interactive-skip-comments nil
  "If non-nil, strip comment lines before sending input to the magma process."
  :group 'magma
  :set 'magma-set-skip
  :type 'boolean)

(defcustom magma-interactive-method 'line
  "How should we send instructions to the magma process?

Can be one of the following symbols
   - 'whole : send all at once
   - 'expr  : send one expression at a time
   - 'line  : send one line at a time
   - 'file : write the region to a temporary file, and use
     magma's load feature to evaluate it

  If `magma-interactive-wait-between-inputs' is nil, this
  setting does not change anything to the visible
  result.  However, it should prevent some edge cases, when the
  input is too long to be sent to the magma process at once.  In
  this case, `emacs' will cut the input in half at an arbitrary
  location, effectively confusing `magma'.  Sending line per line
  or expression per expression reduces the risk of having too
  long input by forcing cuts at syntactically correct places."

  :group 'magma
  :options '(whole expr line file)
  :type 'symbol)

(defvar magma-temp-file-name "/tmp/magma_temp.m")

(defcustom magma-interactive-use-load nil
  "See `magma-eval-buffer'."
  :group 'magma
  :type 'boolean)

(defcustom magma-interactive-auto-save 'confirm
  "Auto-save before sending-input?

This variable controls what to do if
`magma-interactive-use-load' is non-nil, and if the current
buffer has been modified.  It takes one of the following three values:
- `confirm' : ask for confirmation (default)
- `always' : always save, no confirmation
- `never' : never save, no confirmation"
  :group 'magma
  :options '(confirm always never)
  :type 'symbol)

(defcustom magma-interactive-wait-between-inputs t
  "Wait for output before sending next input?

If non nil and `magma-interactive-method' is set to `expr' or
`line', wait for the magma process to output before sending the
next input.

With the current implementation, this should not induce any
overhead, so this variable is set to t by default.  If you need
to set it to nil, please file an issue explaining why, it is
likely to be a bug or a design flaw.

Setting this variable has no effect in term mode."
  :group 'magma
  :type 'boolean)

;; (defvar magma--comint-interactive-escape-map
;;   (if magma-interactive-comint-emulates-term
;;       (let ((map (copy-keymap ctl-x-map)))
;;         (define-key map (kbd "C-c C-c") 'comint-interrupt-subjob)
;;         map)
;;     nil))

(defvar magma-comint-interactive-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map )))
    (define-key map "\t" 'completion-at-point)
    (define-key map (kbd "RET") 'comint-send-input)
    (define-key map (kbd "C-a") 'comint-bol-or-process-mark)
    (define-key map (kbd "C-c p") 'magma-comint-toggle-pause-output)
    ;(define-key map (kbd "C-c") magma--comint-interactive-escape-map)
    map)
  "Keymap for magma-interactive-mode.")

(defvar magma-term-interactive-mode-map
  (let ((map (nconc (make-sparse-keymap) term-mode-map)))
    map)
  "Keymap for magma-interactive-mode.")


(defconst magma-prompt-regexp "^[^ <\n]*>"
  "Regexp matching the magma prompt.")

(defvar magma-prompt-read-only t
  "Should the prompt in the magma buffer be read-only?

Setting this to nil can be confusing to users, but we expose
this setting for elisp calls.")

;; (defvar-local magma--output-finished t)

(defcustom magma-interactive-use-comint t
    "If non-nil, communication with the magma process is done using comint.

Otherwise, it uses ‘term-mode’.  After setting this variable,
restarting Emacs is required (or reloading the magma-mode load
file)."
    :group 'magma
    :type 'boolean)

;; (defcustom magma-interactive-comint-emulates-term nil
;;   "Should comint buffers try to emulate term buffers?

;; If non-nil, interactive buffers using comint-mode try to emulate
;; the behavior of term-mode buffers. At the moment, it only means
;; that `C-c' can be used as a synonym for `C-x' (e.g. `C-c o' for
;; other-buffer), with the exception of `C-c C-c' which remains
;; bound to comint-interrupt-subjob.

;; After setting this variable,
;; restarting emacs is required (or reloading the magma-mode load
;; file)."
;;   :group 'magma
;;   :type 'boolean)



(defun magma-get-buffer-name (&optional i app)
  (let ((name
         (if app (concat magma-interactive-buffer-name "-" app)
           magma-interactive-buffer-name)))
    (if (not i) (magma-get-buffer-name magma-working-buffer-number)
      (if (integerp i)
          (if (= i 0) name
            (concat name "-" (int-to-string i)))
        (concat name "-" i)))))

(defun magma-set-working-buffer (i)
  "Set the I'th buffer as the working buffer."
  (interactive "NBuffer number ?: ")
  (save-window-excursion
    (magma-run i))
  (setq magma-working-buffer-number i)
  (message (concat "Working buffer set to " (int-to-string i)))
  (magma-get-buffer i))

(defun magma-set-working-buffer-locally ()
  "Change the working buffer number."
  (interactive)
  (make-local-variable 'magma-working-buffer-number)
  (call-interactively #'magma-set-working-buffer))

(defun magma-make-buffer-name (&optional i app)
  "Return the name of the I'th magma buffer.

If APP is not nil, it is a string which will be added before
the buffer number."
  (concat "*" (magma-get-buffer-name i app) "*"))

(defun magma-get-buffer (&optional i norun)
  "Return the I'th magma buffer.

If optional arg NORUN is nil (the default) and the buffer
doesn't exist or doesn't have a running process, start a magma
process.  If NORUN is t, in that case, return nil."
  (if norun
      (get-buffer (magma-make-buffer-name i))
    (magma-run i)))
;; magma-run only returns the buffer if it exists and has a process, and runs a process
                
(defcustom magma-interactive-prompt t
  "If non nil, prompt for the path to the magma program and its arguments."
  :group 'magma)

(defun magma--interactive-read-spec (prog args)
  "Read the program to run and the arguments, if needed.

If `magma-interactive-prompt' is set to `t', prompt the user for
the command line, and split it in words. Otherwise, return the
program and arguments given as input.

The output is a list whose car is the program and cdr is the
arguments.

This function is meant for internal use only."
  (if magma-interactive-prompt
      (let* ((default (concat prog (or args "")))
             (prompt "Program to run: ")
             (readval (read-string prompt default nil default)))
        (split-string readval))
    (cons prog args)))

;; Comint definitions

(defun magma-comint-run (&optional i)
  "Run an inferior instance of magma inside emacs, using comint."
  (let ((bufname (magma-make-buffer-name i)))
    (unless (comint-check-proc bufname) ; The buffer is new
      (let* ((file (buffer-file-name))
             (directory (or magma-default-directory
                            (and file (f-dirname file))
                            "~/"))
             (progargs (magma--interactive-read-spec
                        magma-interactive-program
                        magma-interactive-arguments))
             (program (car progargs))
             (args (cdr progargs))
             (new-interactive-buffer
              (apply #'make-comint-in-buffer
                     (magma-get-buffer-name i)
                     (magma-make-buffer-name i)
                     program
                     nil
                     args)))
        (push (or i 0) magma-active-buffers-list)
        (set-buffer new-interactive-buffer)
        (cd directory)
        (setq magma-pending-input (magma-q-create))
        (setq magma-ready t)
        (magma-interactive-mode)
        (when (not (comint-check-proc bufname))
          (error (format "Failed to start process '%s'" program)))
        (magma-comint-send-string
         (concat "ChangeDirectory(\"" directory "\");") i t)))
    (with-current-buffer bufname (current-buffer))))

(defun magma-comint-int (&optional i)
  "Interrupt the magma process in buffer I."
  (with-current-buffer (magma-get-buffer i t)
    (setq magma-active-buffers-list
          (remove (or i 0) magma-active-buffers-list))
    (or (not (comint-check-proc (current-buffer)))
        (comint-interrupt-subjob))
    (setq magma-ready t)
    (setq magma-pending-input (magma-q-create))))

(defun magma-comint-kill (&optional i)
  "Kill the magma process in buffer I."
  ;;(interactive "P")
  (setq magma-active-buffers-list
        (remove (or i 0) magma-active-buffers-list))
  (let ((buf (magma-get-buffer i t)))
    (when buf
      (with-current-buffer buf
        (or (not (comint-check-proc (current-buffer)))
            (comint-kill-subjob))
        (setq magma-ready t)
        (setq magma-pending-input (magma-q-create))))))

(defun magma-comint-toggle-pause-output ()
  "Pause the autoscroll in the comint buffer."
  (interactive)
  (setq comint-scroll-to-bottom-on-output
        (not comint-scroll-to-bottom-on-output)))

(defun magma-comint-send-string (expr &optional i norun)
  "Send the expression EXPR to the magma buffer for evaluation.

If optional NORUN is t, do not attempt to start a process."
  (let ((command (concat expr "\n")))
    (run-hook-with-args 'comint-input-filter-functions expr)
    (comint-send-string (magma-get-buffer i norun) command)))

(defun magma-comint-send (expr &optional i)
  "Send the expression EXPR to the magma buffer for evaluation.

If the magma process is currently processing some previous input,
pushes `expr' onto the `magma-pending-input' queue."
  (let ((buffer (magma-get-buffer i))
        (oldbuf (current-buffer)))
    (with-current-buffer buffer
      (if magma-ready
          (progn
            (setq magma-ready nil)
            (setq magma-timer (current-time))
            (magma-comint-evaluate-here expr))
        (magma-q-push magma-pending-input expr)))
    (or
     ; First try for a window in same frame
     (display-buffer-reuse-window buffer nil)
     ; Then a window in another frame
     (display-buffer-reuse-window buffer '((reusable-frames .  t)))
     ; Then pop the buffer in another window
     (pop-to-buffer buffer))
    (select-window (get-buffer-window oldbuf))))

;; (defun magma--comint-get-old-input-before-send ()
;;   "Function for the variable 'comint-get-old-input

;; This function is supposed to be the content of the variable
;; 'comint-get-old-input when evaluating content in the regular
;; way. In that case, if the point is not at the current input line,
;; we *don't* want input to take into account the current of the
;; magma buffer around the point.
;; "
;;   (save-excursion
;;     (comint-goto-process-mark)
;;     (comint-get-old-input-default)
;;     )
;;   )

(defun magma-comint-next-input (string)
  "Send next input if the buffer is ready for it.

This function should only be called when the current buffer is a
magma evaluation buffer."
  ;; (message (concat "-> " string " <-"))
  ;; (setq magma-ready t)
  (save-excursion
    (goto-char (point-max))
    (when (or
         (not magma-interactive-wait-between-inputs)
         ;; (string-match-p "\(.*\n\)?[^ <\n]*> $" string)
         ;;(looking-back "> " (- (point) 2))
         ;;(string-match-p ".*> $" string)
         (<= (point) 2)
         (save-excursion
           (forward-char -2)
           (looking-at "> ")
         )
         )
    (if (magma-q-is-empty? magma-pending-input)
        (setq magma-ready t)
      ;(let ((comint-get-old-input 'magma--comint-get-old-input-before-send))
        (magma-comint-evaluate-here (magma-q-pop magma-pending-input))))))

         
(defun magma-comint-evaluate-here (expr)
  "Evaluate the expression EXPR in the current buffer.

This function should only be called when the current buffer is a
magma evaluation buffer."
  (save-excursion
    (let ((command (magma-preinput-filter expr)))
                                        ; (unless (s-equals? command "")
      (run-hook-with-args 'comint-input-filter-functions command)
                                        ;(goto-char (point-max))
      (comint-goto-process-mark)
      (insert command)
      (comint-send-input))))
  
(defun magma-comint-help-word (topic)
  "Call-up the handbook in an interactive buffer for TOPIC."
  (interactive "sMagma help topic: ")
  (make-comint-in-buffer (magma-get-buffer-name "help")
                         (magma-make-buffer-name "help")
                         magma-interactive-program
                         magma-interactive-arguments)
  (with-current-buffer (magma-get-buffer "help")
    (magma-interactive-mode))
  (comint-send-string
   (magma-get-buffer "help")
   (format "?%s\n" topic))
  (display-buffer (magma-get-buffer "help")))

(defun magma-comint-send-now (expr &optional i)
  "Prompt for an expression then send it immediately.

The expression is sent to the comint buffer without waiting until
ready.

The primary use-case is to be able to send a value to a `read' or
`readi' prompt, without having to switch buffers.

This can be used to force a new line feed when the magma prompt
is hanging at the end of the line, preventing comint to
acknowledge that the magma process is ready."
  (interactive "sExpression: ")
  (let ((buf (magma-get-buffer (magma-choose-buffer i))))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-max))
        (insert expr)
        (comint-send-input)))))
   

;; Term-mode definitions

(defun magma-term-run (&optional i)
  "Run an inferior instance of magma inside emacs, using term."
  (let* ((magma-buffer-name (magma-get-buffer-name i))
         (reusing-buff
          (get-buffer-process (concat "*" magma-buffer-name "*")))
         (new-interactive-buffer
          (make-term magma-buffer-name magma-interactive-program)))
    (save-excursion
      (push (or i 0) magma-active-buffers-list)
      (set-buffer new-interactive-buffer)
      (unless reusing-buff
        (insert
         (concat "// WARNING: term mode for the magma interactive buffer is\n"
                 "// deprecated and may be removed from future releases.\n"
                 "// You can activate comint mode (recommended) by setting\n "
                 "// the variable `magma-interactive-use-comint' to `t'.\n"
                 "// If you encounter problems with comint but not with term,\n"
                 "// please report them by mail or through the issue tracker.\n")))
      (magma-interactive-mode)
      (term-char-mode))))
    
(defun magma-term-int (&optional i)
  "Interrupt the magma process in buffer I."
  (setq magma-active-buffers-list
        (remove (or i 0) magma-active-buffers-list))
  (if (term-check-proc (magma-get-buffer i))
      (with-current-buffer (magma-get-buffer i)
        (term-send-string (magma-get-buffer i) "\C-c"))))

(defun magma-term-kill (&optional i)
  "Kill the magma process in buffer I."
  (setq magma-active-buffers-list
        (remove (or i 0) magma-active-buffers-list))
  (if (term-check-proc (magma-get-buffer i))
      (with-current-buffer (magma-get-buffer i)
        (term-kill-subjob))))


(defun magma-term-send (expr &optional ins)
  "Send the expression EXPR to the magma buffer for evaluation."
  (save-window-excursion
    (let ((command (magma-preinput-filter expr)))
      ;(unless (s-equals? command "")
        (magma-switch-to-interactive-buffer)
        (goto-char (point-max))
        (insert command)
        (term-send-input))))
;)

(defun magma-term-help-word (topic)
  "Call-up the handbook in an interactive buffer for TOPIC."
  (interactive "sMagma help topic: ")
  (make-term (magma-get-buffer-name "help")
             magma-interactive-program)
  (with-current-buffer (magma-get-buffer "help")
    (magma-interactive-mode)
    (term-line-mode)
    (term-show-maximum-output))
  (term-send-string
   (magma-get-buffer "help")
   (format "?%s\n" topic))
  (display-buffer (magma-get-buffer "help")))


;; Wrappers

(defun magma-send-expression (expr &optional i)
  "Send the expression."
  (interactive "iP")
  (let* ((initval (if (region-active-p)
                      (buffer-substring-no-properties (region-beginning)
                                                      (region-end))))
         (expr
          (or expr
              (read-string "Expr:" initval))))
    (magma-send-or-broadcast expr i)))

(defun magma-restart (&optional i)
  "Restart the magma process in buffer I."
  (interactive "P")
  (magma-kill-or-broadcast i)
  (sleep-for 2)
  (magma-run-or-broadcast i))

(defun magma-switch-to-interactive-buffer (&optional i)
  "Switch to the magma process in buffer I in another frame."
  (interactive "P")
  (magma-run i)
  (switch-to-buffer-other-frame (magma-get-buffer i)))

(defun magma-switch-to-interactive-buffer-same-frame (&optional i)
  "Switch to the magma process in buffer I, in another window on the same frame."
  (interactive "P")
  (magma-run i)
  (pop-to-buffer (magma-get-buffer i)))

(defun magma--at-end (end)
  (or (looking-at "\\([[:blank:]]\\|\n\\)*\\'")
                  (>= (point) end)))
    
(defun magma-eval-region (beg end &optional i)
  "Evaluate the current region.

The behavior of this function depends on the value of
`magma-interactive-method':
- if `whole', send the whole region to comint. Emacs may decide
  that this block of text is too long for input, and cut it and
  send it in batches. In this case, it will cut it
  at (apparently) random points, which may cause syntax errors, if
  the break is in the middle of an identifier for example;
- if `line', send the region one line at a time;
- if `expr', send the region one expr at a time;
- if `file', copy the region to a temporary file and use \"load ...;\"
  to evaluate it in magma.

Additionally, if `magma-interactive-wait-between-inputs' is non
nil, and if `magma-interactive-method' is either `line' or
`expr', emacs will wait until magma has processed the input
before sending the next part. The result is that the buffer is
more nicely structured, with each output located right after the
corresponding input."
  (interactive "rP")
  (save-restriction
    (narrow-to-region beg end)
    (cl-case magma-interactive-method
      ('whole
       (let ((str (buffer-substring-no-properties beg end)))
         (magma-send-or-broadcast str i)))
      ('expr
       (save-excursion
         (goto-char beg)
       (let ((magma-interactive-method 'whole))
         (while (not (magma--at-end end))
           (magma-eval-next-statement i)))))
    ('line
     (save-excursion
       (goto-char beg)
       (while (not (magma--at-end end))
         (magma-eval-line i))))
    ('file
     (let ((buf (current-buffer)))
       (with-temp-buffer
         (find-file-literally magma-temp-file-name)
         (erase-buffer)
         (insert-buffer-substring-no-properties buf beg end)
         (let ((magma-interactive-use-load t)
               (magma-interactive-auto-save 'always))
           (magma-eval-buffer i))
         (kill-buffer)))))))

(defun magma-eval-line ( &optional i)
  "Evaluate current line."
  (interactive "P")
  (while (looking-at "^$")
    (forward-line))
  (let* ((beg (save-excursion
                (back-to-indentation)
                (point)))
         (end (save-excursion
                (end-of-line)
                (point))))
    ;; (message (format "eval-line: %s %s" beg end))
    (let ((magma-interactive-method 'whole))
      (magma-eval-region beg end i))
    (end-of-line)
    (or (eobp) (forward-line 1))))

(defun magma-eval-paragraph ( &optional i)
  "Evaluate current paragraph (space separated block)."
  (interactive "P")
  (forward-paragraph)
  (let ((end (point)))
    (backward-paragraph)
    (magma-eval-region (point) end i)
    (goto-char end)))

(defun magma-end-of-expr-or-line ()
  "Go to the end of line or expression according to the evaluation method.

If `magma-interactive-method' is `line', go to the end of the
line, otherwise the end of the expression."
  (interactive)
  (magma-end-of-expr)
  (if (eq magma-interactive-method 'line)
      (progn (forward-line 1)
             (forward-char -1)
             (or (looking-at "$") (forward-char 1)))))

(defun magma-eval-next-statement ( &optional i)
  "Evaluate current or next statement."
  (interactive "P")
  (let ((regbeg (progn
                  (magma-beginning-of-expr)
                  (point)))
        (regend (progn
                  (magma-end-of-expr-or-line)
                  (point))))
    (magma-eval-region regbeg regend i)
    (goto-char regend)
    (or (eobp) (forward-char 1))))
  

(defun magma-eval (&optional i)
  "Evaluate dwim.

The function evaluates the current region if set and the current
statement otherwise"
  (interactive "P")
  (if mark-active
      (magma-eval-region (region-beginning) (region-end) i)
    (progn
      (magma-eval-next-statement i)
      ;(when (looking-at "[[:space:]]*$") (forward-line 1))
      )))

(defun magma-eval-defun (&optional i)
  "Evaluate the current defun."
  (interactive "P")
  (let ((regbeg (progn
                  (magma-beginning-of-defun)
                  (point)))
        (regend (progn
                  (magma-end-of-defun)
                  (point))))
    (magma-eval-region regbeg regend i)))
  

(defun magma-eval-until ( &optional i)
  "Evaluates all code from the beginning of the buffer to the point."
  (interactive "P")
  (magma-eval-region (point-min) (point) i))

(defun magma-eval-buffer ( &optional i)
  "Evaluates all code in the buffer.

If `magma-interactive-use-load' is non-nil and if the current
buffer is associated to a file, use \"load ...;\" to evaluate the
buffer, instead of sending it line per line. Note that if you use
magma on a remote host, this method will require that you save
the file to the remote host when needed.

If needed, confirm saving through `magma-confirm-save-buffer'.

Otherwise, send the whole buffer to `magma-eval-region'."
  (interactive "P")
  (if (and (buffer-file-name) magma-interactive-use-load)
      (progn
        (when (buffer-modified-p)
          (magma-confirm-save-buffer))
        (magma-send-or-broadcast
         (format "load \"%s\";"
                 ; (f-relative (buffer-file-name) magma-working-directory)
                 ;; ^ This would work if magma-working-directory was
                 ;;   correctly kept up to date. Instead, as a fallback, we use:
                 (f-long (buffer-file-name))
                 ) i))
  (magma-eval-region (point-min) (point-max) i)))

(defun magma-confirm-save-buffer ()
  "Prompt for confirmation, then save the current buffer.

The behavior of this function is controlled by
`magma-interactive-auto-save'."
  (let ((should-save
         (cl-case magma-interactive-auto-save
           ('always t)
           ('never nil)
           ('confirm
            (let ((prompt (format "File %s modified. Save? "
                                  (buffer-file-name))))
              (yes-or-no-p prompt))))))
  (when should-save
    (save-buffer))))

(defun magma-help-word (&optional browser)
  "Call-up the handbook in the interactive buffer for the current word."
  (interactive "P")
  (let ((topic (read-string
                (format "Magma help topic (default %s): " (current-word))
                nil nil (current-word))))
    (if browser
        (magma-help-word-browser topic)
      (magma-help-word-text topic))))

(defun magma-help-word-browser (&optional topic)
  "Open the magma help page in a web browser for TOPIC."
  (interactive)
  (let ((topic (or topic
                   (read-string
                    (format "Magma help topic (default %s): " (current-word))
                    nil nil (current-word)))))
    (let ((urlprefix "http://magma.maths.usyd.edu.au/magma/handbook/")
          (urlsuffix "&chapters=1&examples=1&intrinsics=1"))
      (browse-url (concat urlprefix "search?query=" topic urlsuffix)))))


(defun magma-show-word (&optional i)
  "Show the current word in magma."
  (interactive "P")
  (let ((word (current-word)))
    (magma-run-or-broadcast i)
    (magma-send-or-broadcast
     (concat word ";") i)))

(defun magma-broadcast-fun (fun)
  (mapc
   #'(lambda (i) (save-excursion (funcall fun i)))
   magma-active-buffers-list))

(defun magma-choose-buffer (i)
  "Choose what buffer to work on (internal).

Given an input I in raw prefix form, decides what buffers we
should be working on.  The input can be an integer, in which
case it returns that integer; or it can be the list '(4), in
which case it prompts for an integer; or it can be the list
'(16), in which case it returns the symbol 'broadcast, meaning
we should work on all buffers"
  (cond
   ((not i) magma-working-buffer-number)
   ((integerp i)
    i)
   ((and (listp i) (eq (car i) 4))
    (read-number "In buffer?" magma-working-buffer-number))
   ((and (listp i) (eq (car i) 16))
    'broadcast)
   (t (message "Invalid buffer specified") magma-working-buffer-number)))

(defun magma-broadcast-if-needed (fun i)
  (let ((buf (magma-choose-buffer i)))
    (cond
     ((integerp buf)
      (funcall fun buf))
     ((eq buf 'broadcast)
      (magma-broadcast-fun fun))
     (t (message "Invalid buffer specified")))))

(defun magma-send-or-broadcast (expr i)
  (magma-broadcast-if-needed (apply-partially 'magma-send expr) i))
(defun magma-kill-or-broadcast (i)
  (magma-broadcast-if-needed 'magma--kill-cmd i))
(defun magma-int-or-broadcast (i)
  (magma-broadcast-if-needed 'magma--int-cmd i))
(defun magma-run-or-broadcast (i)
  (magma-broadcast-if-needed 'magma-run i))

(defun magma-kill (i)
  "Kill the magma process."
  (interactive "P")
  (magma-kill-or-broadcast i))

(defun magma-int (i)
  "Interrupt the magma process."
  (interactive "P")
  (magma-int-or-broadcast i))

;;(defvar magma--echo-complete nil)

;; (defun magma-message-raw-output (output)
;;   (message output)
;;   output)

(defun magma-comint-delete-reecho (output)
  ;(message output)
  (with-temp-buffer
    (insert output)
    (flush-lines "\" (point-min) (point-max))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun magma-preinput-filter (input)
  (with-temp-buffer
    (insert input)
    (let ((magma-mode-hook nil))
      (magma-mode))
    (when magma-interactive-skip-comments
      (goto-char (point-min))
      (insert "\n")
      (magma--comment-kill-no-kill-ring (count-lines (point-min) (point-max))))
    (when magma-interactive-skip-empty-lines
      (flush-lines "^[[:blank:]]*$" (point-min) (point-max)))
    (buffer-substring-no-properties (point-min) (point-max))))

;; (defun magma-comint-delete-reecho (output)
;;   (when (string-match "^\\(\\(.\\|\n\\)*\n\\)[[:alnum:]|]*> \\1" output)
;;     (setq output (replace-match "" nil nil output)))
;;   output)

(defun magma-comint-send-input ()
  "Obsolete."
  (interactive)
  (message "`magma-comint-send-input' is deprecated, use `comint-send-input' instead.")
  (comint-send-input))

(defconst magma-interactive-modeline-ready-face
  "success")
(defconst magma-interactive-modeline-run-face
  "warning")
(defconst magma-interactive-modeline-stop-face
  "error")

(defun magma-interactive-make-mode-line-process ()
  (format
   ":%s"
   (if (comint-check-proc (current-buffer))
       (if magma-ready
           (propertize "ready" 'face magma-interactive-modeline-ready-face)
         (concat
          (propertize "run" 'face magma-interactive-modeline-run-face)
          ":["
          (format-seconds
          "%d:%h:%02m:%z%02s]"
          (float-time (time-subtract (current-time) magma-timer)))))
     (propertize "stop" 'face magma-interactive-modeline-stop-face))))

(defun magma-interactive-common-settings ()
  "Settings common to comint and term modes."
  ;; Compilation shell mode
  (add-to-list
   'compilation-error-regexp-alist
   '("^In file \"\\(.*?\\)\", line \\([0-9]+\\), column \\([0-9]+\\):$"
     1 2 3 2 1))
  (setq-local compilation-mode-font-lock-keywords nil)
  (compilation-shell-minor-mode 1)
  ;; Mode line name
  (setq mode-name "Magma-eval")
  (setq mode-line-process '(:eval (magma-interactive-make-mode-line-process))))

(define-derived-mode magma-comint-interactive-mode
  comint-mode
  "Magma-Interactive"
  "Magma interactive mode (using comint)
\\<magma-comint-interactive-mode-map>"
  (setq-local comint-use-prompt-regexp nil)
  (setq-local comint-prompt-read-only magma-prompt-read-only)
  (setq-local comint-prompt-regexp magma-prompt-regexp)
  (setq-local comint-scroll-to-bottom-on-output t)
  (setq-local comint-input-ignoredups t)
  ;(add-hook 'comint-preoutput-filter-functions 'magma-message-raw-output nil t)
  (add-hook 'comint-preoutput-filter-functions 'magma-comint-delete-reecho nil t)
  (add-hook 'comint-output-filter-functions 'magma-comint-next-input nil t)
  (magma-interactive-common-settings)
  )

(define-derived-mode magma-term-interactive-mode
  term-mode
  "Magma-Interactive"
  "Magma interactive mode (using term)
\\<magma-term-interactive-mode-map>"
  (setq-local term-scroll-to-bottom-on-output t)
  (setq-local font-lock-defaults '(magma-interactive-font-lock-keywords nil nil))
  (magma-interactive-common-settings))


(defun magma-interactive-init-with-comint ()
  (defalias 'magma-interactive-mode 'magma-comint-interactive-mode)
  (defalias 'magma-run 'magma-comint-run)
  (defalias 'magma--int-cmd 'magma-comint-int)
  (defalias 'magma--kill-cmd 'magma-comint-kill)
  (defalias 'magma-send 'magma-comint-send)
  (defalias 'magma-help-word-text 'magma-comint-help-word))

(defun magma-interactive-init-with-term ()
  (defalias 'magma-interactive-mode 'magma-term-interactive-mode)
  (defalias 'magma-run 'magma-term-run)
  (defalias 'magma--int-cmd 'magma-term-int)
  (defalias 'magma--kill-cmd 'magma-term-kill)
  (defalias 'magma-send 'magma-term-send)
  (defalias 'magma-help-word-text 'magma-term-help-word)
  )

(defun magma-interactive-init ()
  (if magma-interactive-use-comint
      (magma-interactive-init-with-comint)
    (magma-interactive-init-with-term)))


(provide 'magma-interactive)

;;; magma-interactive.el ends here
