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

(require 'magma-completion)

(defcustom magma-interactive-program "magma"
  "*Program to be launched to use magma (usually magma)"
  :group 'magma
  :type 'string)

(defcustom magma-interactive-arguments '()
  "Commandline arguments to pass to magma"
  :group 'magma
  :type 'sexp)

(defvar magma-working-buffer-number 0
  "Should this buffer send instructions to a different magma buffer")

(defvar magma-active-buffers-list '()
  "*List of active magma buffers.")

(defcustom magma-interactive-buffer-name "magma"
  "*Name of the buffer to be used for using magma
  interactively (will be surrounded by `*')"
  :group 'magma
  :type 'string)

(defcustom magma-interactive-skip-empty-lines nil
  "If non-nil, strip empty lines before sending input to the
  magma process.

  This variable can be set to `nil' even if
  `magma-interactive-skip-comments' is set to `t'. However,
  this will probably lead to unwanted behavior, since the
  commented lines are replaced with blank lines."
  :group
  'magma :type 'sexp)

(defcustom magma-interactive-skip-comments nil
  "If non-nil, strip empty lines before sending input to the
  magma process"
  :group 'magma
  :type 'sexp)

(defcustom magma-interactive-method 'line
  "How should we send instructions to the magma process:
   - 'whole : send all at once
   - 'expr  : send one expression at a time
   - 'line  : send one line at a time.

  If `magma-interactive-wait-between-inputs' is `nil', this
  setting does not change anything to the visible
  result. However, it should prevent some edge cases, when the
  input is too long to be sent to the magma process at once. In
  this case, `emacs' will cut the input in half at an arbitrary
  location, effectively confusing `magma'. Sending line per line
  or expression per expression reduces the risk of having too
  long input by forcing cuts at syntactically correct places."

  :group 'magma
  :options '(whole expr line)
  :type 'symbol)

(defcustom magma-interactive-wait-between-inputs nil
  "If non nil and `magma-interactive-method' is set to `expr' or
  `line', wait for the magma process to output before sending the
  next input.

  It can make the evaluation of a long buffer slower by a few
  seconds."
  :group 'magma
  :type 'sexp)

(defvar magma-comint-interactive-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    (define-key map "\t" 'completion-at-point)
    (define-key map (kbd "RET") 'comint-send-input)
    (define-key map (kbd "C-a") 'comint-bol-or-process-mark)
    map)
  "Keymap for magma-interactive-mode")

(defvar magma-term-interactive-mode-map
  (let ((map (nconc (make-sparse-keymap) term-mode-map)))
    map)
  "Keymap for magma-interactive-mode")


(defvar magma-prompt-regexp "^[^ ]*> "
  "Regexp matching the magma prompt")

(defvar magma-prompt-read-only t
  "Should the prompt in the magma buffer be read-only? Setting
  this to `nil' can be confusing to users, but we expose this
  setting for elisp calls.")

;; (defvar-local magma--output-finished t)

(defcustom magma-interactive-use-comint nil
  "If non-nil, communication with the magma process is done using comint. Otherwise, it uses term-mode.

After changing this variable, restarting emacs is required (or reloading the magma-mode load file)."
  :group 'magma
  :type 'boolean)

(defun magma-get-buffer-name (&optional i)
  (if (not i) (magma-get-buffer-name magma-working-buffer-number)
    (if (integerp i)
        (if (= i 0) magma-interactive-buffer-name
          (concat magma-interactive-buffer-name "-" (int-to-string i)))
      (concat magma-interactive-buffer-name "-" i))))

(defun magma-set-working-buffer (i)
  "set the i-th buffer as the working buffer"
  (interactive "NBuffer number ?: ")
  (magma-run i)
  (setq magma-working-buffer-number i)
  (message (concat "Working buffer set to " (int-to-string i)))
  (magma-get-buffer i))

(defun magma-make-buffer-name (&optional i)
  "Return the name of the i-th magma buffer"
  (concat "*" (magma-get-buffer-name i) "*"))

(defun magma-get-buffer (&optional i)
  "return the i-th magma buffer"
  (get-buffer (magma-make-buffer-name i)))

;; Comint definitions

(defun magma-comint-run (&optional i)
  "Run an inferior instance of magma inside emacs, using comint."
  ;;(interactive)
  (let* ((default-directory
           ;;(if
           ;; (or
           ;;  (not (file-remote-p default-directory)) 
           ;;  (tramp-sh-handle-executable-find magma-interactive-program))
           ;;   default-directory
            magma-default-directory
            ;;   )
         )
         (new-interactive-buffer
          (progn
;;            (set-buffer (magma-make-buffer-name i))
            ;; ^ Force default-directory to be taken into account if needed
            (make-comint-in-buffer (magma-get-buffer-name i)
                                   (magma-make-buffer-name i)
                                   magma-interactive-program
                                   magma-interactive-arguments
                                   )
            )))
    (if (not (memq (or i 0) magma-active-buffers-list))
        (push (or i 0) magma-active-buffers-list))
    (set-buffer new-interactive-buffer)
    ;;(magma-send "SetIgnorePrompt(true);")
    (magma-interactive-mode)
  ))

(defun magma-comint-int (&optional i)
  "Interrupt the magma process in buffer i"
  ;;(interactive "P")
  (set-buffer (magma-get-buffer i))
  ;;(comint-interrupt-subjob)
  (or (not (comint-check-proc (current-buffer)))
      (interrupt-process nil comint-ptyp))
  ;; ^ Same as comint-kill-subjob, without comint extras.
  )

(defun magma-comint-kill (&optional i)
  "Kill the magma process in buffer i"
  ;;(interactive "P")
  (set-buffer (magma-get-buffer i))
  ;;(comint-kill-subjob)
  (or (not (comint-check-proc (current-buffer)))
      (kill-process nil comint-ptyp))
  ;; ^ Same as comint-kill-subjob, without comint extras.
  )

(defun magma-comint-send-string (expr &optional i)
  "Send the expression expr to the magma buffer for evaluation."
  (let ((command (concat expr "\n")))
    (run-hook-with-args 'comint-input-filter-functions expr)
    (comint-send-string (magma-get-buffer i) command))
    )

(defun magma-comint-send (expr &optional i)
  "Send the expression expr to the magma buffer for evaluation."
  (let ((command (magma-preinput-filter expr))
        (buffer (magma-get-buffer i)))
    (run-hook-with-args 'comint-input-filter-functions expr)
    (with-current-buffer buffer
      (end-of-buffer)
      ;; (goto-char (process-mark (get-buffer-process buffer)))
      (insert command)
      ;; (setq magma--output-finished t)
      (comint-send-input))))


(defun magma-comint-help-word (topic)
  "call-up the handbook in an interactive buffer for topic"
  (interactive "sMagma help topic: ")
  (make-comint-in-buffer (magma-get-buffer-name "help")
                         (magma-make-buffer-name "help")
                         magma-interactive-program
                         magma-interactive-arguments)
  (save-excursion
    (set-buffer (magma-get-buffer "help"))
    (magma-interactive-mode)
    )
  (comint-send-string
   (magma-get-buffer "help")
   (format "?%s\n" topic))
  (display-buffer (magma-get-buffer "help")))

   

;; Term-mode definitions

(defun magma-term-run (&optional i)
  "Run an inferior instance of magma inside emacs, using term."
  (let ((new-interactive-buffer
         (make-term (magma-get-buffer-name i) magma-interactive-program)))
    (save-excursion
      (if (not (memq (or i 0) magma-active-buffers-list))
          (push (or i 0) magma-active-buffers-list))
      (set-buffer new-interactive-buffer)
      (magma-interactive-mode)
      (term-char-mode))))
    
(defun magma-term-int (&optional i)
  "Interrupt the magma process in buffer i"
  ;;(interactive "P")
  (if (term-check-proc (magma-get-buffer i))
      (save-excursion
        (set-buffer (magma-get-buffer i))
        (term-send-string (magma-get-buffer i) "\C-c"))))

(defun magma-term-kill (&optional i)
  "Kill the magma process in buffer i"
  ;;(interactive "P")
  (if (term-check-proc (magma-get-buffer i))
      (save-excursion
        ;; (setq magma-active-buffers-list
        ;;       (delq (or i 0) magma-active-buffers-list))
        (set-buffer (magma-get-buffer i))
        (term-kill-subjob))))


(defun magma-term-send (expr &optional ins)
  "Send the expression expr to the magma buffer for evaluation."
  (save-window-excursion
    (let ((command (magma-preinput-filter expr))
      (magma-switch-to-interactive-buffer)
      (end-of-buffer)
      (insert command)
      (term-send-input)
      ))))

(defun magma-term-help-word (topic)
  "call-up the handbook in an interactive buffer for topic"
  (interactive "sMagma help topic: ")
  (make-term (magma-get-buffer-name "help")
             magma-interactive-program)
  (save-excursion
    (set-buffer (magma-get-buffer "help"))
    (magma-interactive-mode)
    (term-line-mode)
    (term-show-maximum-output))
  (term-send-string
   (magma-get-buffer "help")
   (format "?%s\n" topic))
  (display-buffer (magma-get-buffer "help")))


;; Wrappers

(defun magma-send-expression (expr &optional i)
  (interactive "iP")
  (let* ((initval (if (region-active-p)
                      (buffer-substring-no-properties (region-beginning)
                                                      (region-end))))
         (expr
          (or expr
              (read-string "Expr:" initval))))
    (magma-send-or-broadcast expr i)))

(defun magma-restart (&optional i)
  "Restart the magma process in buffer i"
  (interactive "P")
  (magma-kill-or-broadcast i)
  (sleep-for 2)
  (magma-run-or-broadcast i)
  )

(defun magma-switch-to-interactive-buffer (&optional i)
  "Switch to the magma process in buffer i in another frame"
  (interactive "P")
  (magma-run i)
  (switch-to-buffer-other-frame (magma-get-buffer i))
  )

(defun magma-switch-to-interactive-buffer-same-frame (&optional i)
  "Switch to the magma process in buffer i, in another window on the same frame"
  (interactive "P")
  (magma-run i)
  (pop-to-buffer (magma-get-buffer i))
  )

(defun magma-wait-for-output (&optional i)
  (let ((buffer (magma-get-buffer i)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (forward-line 0) ;; beginning-of-line won't go across the prompt
      (while (not (looking-at "^[[:alnum:]|]*> "))
        (accept-process-output nil 0.001)
        (redisplay)
        (goto-char (point-max))
        (forward-line 0))
      (end-of-line))))

(defun magma-eval-region (beg end &optional i)
  "Evaluates the current region"
  (interactive "rP")
  (let* ((ignore (lambda (i) nil))
         (wait (if magma-interactive-wait-between-inputs
                   'magma-wait-or-broadcast
                 'ignore)))
    (case magma-interactive-method
      ('whole
       (let ((str (buffer-substring-no-properties beg end)))
         (magma-send-or-broadcast str i)))
      ('expr
       (save-excursion
         (goto-char beg)
         (let ((magma-interactive-method 'whole))
           (while (< (point) end)
             (magma-eval-next-statement i)
             (funcall wait i)))))
      ('line
       (save-excursion
         (goto-char beg)
         (while (< (point) end)
           ;; (message (format "Point: %s" (point)))
           (magma-eval-line i)
           (funcall wait i)))))))

(defun magma-eval-line ( &optional i)
  "Evaluate current line"
  (interactive "P")
  (while (looking-at "^$")
    (forward-line))
  (let* ((beg (save-excursion
                (back-to-indentation)
                (point)))
         (end (save-excursion
                (end-of-line)
                (point))))
    (let ((magma-interactive-method 'whole))
      (magma-eval-region beg end i))
    (end-of-line)
    (or (eobp) (next-line))))

(defun magma-eval-paragraph ( &optional i)
  "Evaluate current paragraph (space separated block)"
  (interactive "P")
  (forward-paragraph)
  (let ((end (point)))
    (backward-paragraph)
    (magma-eval-region (point) end i)
    (goto-char end)))

(defun magma-eval-next-statement ( &optional i)
  "Evaluate current or next statement"
  (interactive "P")
  (let ((regbeg (progn
                  (magma-beginning-of-expr)
                  (point)))
        (regend (progn
                  (magma-end-of-expr)
                  (point))))
    (magma-eval-region regbeg regend i)
    (goto-char regend)
    (or (eobp) (next-line))))
  

(defun magma-eval (&optional i)
  "Evaluate the current region if set and the current statement
  otherwise"
  (interactive "P")
  (if mark-active
      (magma-eval-region (region-beginning) (region-end) i)
    (progn
      (magma-eval-next-statement i)
      (when (looking-at "[[:space:]]*$") (forward-line 1)))))

(defun magma-eval-defun (&optional i)
  "Evaluate the current defun"
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
  (magma-end-of-statement-1)
  (magma-eval-region (point-min) (point) i))

(defun magma-eval-buffer ( &optional i)
  "Evaluates all code in the buffer"
  (interactive "P")
  (magma-eval-region (point-min) (point-max) i))

(defun magma-help-word (&optional browser)
  "call-up the handbook in the interactive buffer for the current word"
  (interactive "P")
  (let ((topic (read-string
                (format "Magma help topic (default %s): " (current-word))
                nil nil (current-word))))
    (if browser
        (magma-help-word-browser topic)
      (magma-help-word-text topic))))

(defun magma-help-word-browser (topic)
  "open the magma help page in a web browser for topic"
  (interactive "sMagma help topic: ")
  (let ((urlprefix "http://magma.maths.usyd.edu.au/magma/handbook/")
        (urlsuffix "&chapters=1&examples=1&intrinsics=1"))
    (browse-url (concat urlprefix "search?query=" topic urlsuffix))))


(defun magma-show-word (&optional i)
  "show the current word in magma"
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
  "Given an input i in raw prefix form, decides what buffers we
  should be working on. The input can be an integer, in which
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
(defun magma-wait-or-broadcast (i)
  (magma-broadcast-if-needed 'magma-wait-for-output i))
(defun magma-kill-or-broadcast (i)
  (magma-broadcast-if-needed 'magma--kill-cmd i))
(defun magma-int-or-broadcast (i)
  (magma-broadcast-if-needed 'magma--int-cmd i))
(defun magma-run-or-broadcast (i)
  (magma-broadcast-if-needed 'magma-run i))

(defun magma-kill (i)
  (interactive "P")
  (magma-kill-or-broadcast i))

(defun magma-int (i)
  (interactive "P")
  (magma-int-or-broadcast i))

;;(defvar magma--echo-complete nil)

(defun magma-comint-delete-reecho (output)
  (with-temp-buffer
    (insert output)
    (let ((maxp
           (save-excursion
             (goto-char (point-max))
             (beginning-of-line)
             (if (looking-at "^[[:alnum:]]*>")
                 (progn
                   (or (bobp) (forward-char -1))
                   ;; (setq magma--output-finished t)
                   (point))
               (point-max)))))
      (flush-lines "\\(^[[:alnum:]]*>\\|^[[:blank:]]*$\\|\^H\\)" (point-min) maxp)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun magma-preinput-filter (input)
  (with-temp-buffer
    (insert input)
    (let ((magma-mode-hook nil))
      (magma-mode))
    (when magma-interactive-skip-comments
      (goto-char (point-min))
      (insert "\n")
      (comment-kill (count-lines (point-min) (point-max))))
    (when magma-interactive-skip-empty-lines
      (flush-lines "^[[:blank:]]*$" (point-min) (point-max)))
    (buffer-substring-no-properties (point-min) (point-max))))

;; (defun magma-comint-delete-reecho (output)
;;   (when (string-match "^\\(\\(.\\|\n\\)*\n\\)[[:alnum:]|]*> \\1" output)
;;     (setq output (replace-match "" nil nil output)))
;;   output)

(defun magma-comint-send-input ()
  (interactive)
  (message "`magma-comint-send-input' is deprecated, use `comint-send-input' instead.")
  (comint-send-input))


(defun magma-interactive-common-settings ()
  "Settings common to comint and term modes"
  (compilation-shell-minor-mode 1)
  (set (make-local-variable 'compilation-mode-font-lock-keywords)
        nil)
  (set (make-local-variable 'font-lock-keywords) nil)
  (add-to-list
   'compilation-error-regexp-alist
   '("^In file \"\\(.*?\\)\", line \\([0-9]+\\), column \\([0-9]+\\):$"
     1 2 3 2 1)))

(define-derived-mode magma-comint-interactive-mode
  comint-mode
  "Magma-Interactive"
  "Magma interactive mode (using comint)
\\<magma-comint-interactive-mode-map>"
  ;;(setq comint-process-echoes t)
  ;; This doesn't work because magma outputs the prompting "> ", together
  ;; with the input line.
  (setq comint-use-prompt-regexp nil)
  (setq comint-prompt-read-only magma-prompt-read-only)
  (setq comint-prompt-regexp magma-prompt-regexp)
  (make-local-variable 'comint-highlight-prompt)
  (setq comint-highlight-prompt t)
  ;; (make-local-variable 'comint-highlight-input)
  ;; (setq comint-highlight-input t)
  (setq comint-scroll-to-bottom-on-output t)
  (add-hook 'comint-preoutput-filter-functions 'magma-comint-delete-reecho nil t)
  ;; (make-local-variable 'comint-input-sender)
  ;; (setq comint-input-sender 'magma-comint-input-sender)
  
  (magma-interactive-common-settings)
  ;; (setq font-lock-defaults
  ;;       (list (cons (list "^[[:alnum:]|]*>.*$" 'comint-highlight-input)
  ;;                   magma-interactive-font-lock-keywords)
  ;;             nil nil))
  )  

(define-derived-mode magma-term-interactive-mode
  term-mode
  "Magma-Interactive"
  "Magma interactive mode (using term)
\\<magma-term-interactive-mode-map>"
  (setq term-scroll-to-bottom-on-output t)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(magma-interactive-font-lock-keywords nil nil))
  (magma-interactive-common-settings))


(defun magma-interactive-init-with-comint ()
  (defalias 'magma-interactive-mode 'magma-comint-interactive-mode)
  (defalias 'magma-run 'magma-comint-run)
  (defalias 'magma--int-cmd 'magma-comint-int)
  (defalias 'magma--kill-cmd 'magma-comint-kill)
  (defalias 'magma-send 'magma-comint-send)
  (defalias 'magma-help-word-text 'magma-comint-help-word)
  )

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

