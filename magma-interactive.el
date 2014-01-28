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

(defvar magma-comint-interactive-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    (define-key map "\t" 'completion-at-point)
    (define-key map (kbd "RET") 'magma-comint-send-input)
    (define-key map (kbd "C-a") 'comint-bol-or-process-mark)
    map)
  "Keymap for magma-interactive-mode")

(defvar magma-term-interactive-mode-map
  (let ((map (nconc (make-sparse-keymap) term-mode-map)))
    map)
  "Keymap for magma-interactive-mode")


(defvar magma-prompt-regexp "^[^ ]*> "
  "Regexp matching the magma prompt")

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
  (interactive "P")
  (set-buffer (magma-get-buffer i))
  (comint-interrupt-subjob)
  )

(defun magma-comint-kill (&optional i)
  "Kill the magma process in buffer i"
  (interactive "P")
  (set-buffer (magma-get-buffer i))
  (comint-kill-subjob)
  )

(defun magma-comint-send (expr &optional i)
  "Send the expression expr to the magma buffer for evaluation."
  (let ((command (concat expr "\n")))
    (run-hook-with-args 'comint-input-filter-functions expr)
    (comint-send-string (magma-get-buffer i) command))
    )

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
  (interactive "P")
  (if (term-check-proc (magma-get-buffer i))
      (save-excursion
        (set-buffer (magma-get-buffer i))
        (term-send-string (magma-get-buffer i) "\C-c"))))

(defun magma-term-kill (&optional i)
  "Kill the magma process in buffer i"
  (interactive "P")
  (if (term-check-proc (magma-get-buffer i))
      (save-excursion
        ;; (setq magma-active-buffers-list
        ;;       (delq (or i 0) magma-active-buffers-list))
        (set-buffer (magma-get-buffer i))
        (term-kill-subjob))))


(defun magma-term-send (expr &optional ins)
  "Send the expression expr to the magma buffer for evaluation."
  (save-window-excursion
    (let ((command expr))
      (magma-switch-to-interactive-buffer)
      (end-of-buffer)
      (insert command)
      (term-send-input)
      )))

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
  (interactive "ip")
  (let* ((initval (if (region-active-p)
                      (buffer-substring-no-properties (region-beginning)
                                                      (region-end))))
         (expr
          (or expr
              (read-string "Expr:" initval))))
    (magma-send expr i)))

(defun magma-restart (&optional i)
  "Restart the magma process in buffer i"
  (interactive "P")
  (magma-kill i)
  (sleep-for 2)
  (magma-run i)
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
  (display-buffer (magma-get-buffer i))
  )


(defun magma-eval-region (beg end)
  "Evaluates the current region"
  (interactive "r")
  (let ((str (buffer-substring-no-properties beg end)))
    (magma-send str)
    )
  )

(defun magma-eval-line ()
  "Evaluate current line"
  (interactive)
  (while (looking-at "^$")
    (forward-line))
  (let* ((beg (save-excursion
                (back-to-indentation)
                (point)))
         (end (save-excursion
                (end-of-line)
                (point))))
    (magma-eval-region beg end)
    (next-line)
    )
  )

(defun magma-eval-paragraph ()
  "Evaluate current paragraph (space separated block)"
  (interactive)
  (forward-paragraph)
  (let ((end (point)))
    (backward-paragraph)
    (magma-eval-region (point) end)
    (goto-char end)))

(defun magma-eval-next-statement ()
  "Evaluate current or next statement"
  (interactive)
  (magma-eval-line)
  ;;(magma-eval-region
   ;;(magma-beginning-of-statement-1) (magma-end-of-statement-1))
  )

(defun magma-eval ()
  "Evaluates region if mark is set, else expression."
  (interactive)
  (if mark-active
      (magma-eval-region (region-beginning) (region-end))
    (magma-eval-next-statement))
  )

(defun magma-eval-until ()
  "Evaluates all code from the beginning of the buffer to the point."
  (interactive)
  (magma-end-of-statement-1)
  (magma-eval-region (point-min) (point)))

(defun magma-eval-buffer ()
  "Evaluates all code in the buffer"
  (interactive)
  (magma-eval-region (point-min) (point-max))
)

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
    (magma-run)
    (magma-send
     (concat word ";") i)))


(defun magma-comint-send-input ()
  "Replaces comint-send-input in order to delete the reechoing of
  the input line with its prompt"
  (interactive)
  (let* ((pmark (process-mark
                 (get-buffer-process (current-buffer))))
         (beg (marker-position pmark))
         (end (save-excursion (end-of-line) (point)))
         (str (buffer-substring-no-properties beg end)))
    (delete-region beg end)
    (comint-add-to-input-history str)
    (magma-comint-send str)
    )
  )


(define-derived-mode magma-comint-interactive-mode
  comint-mode
  "Magma-Interactive"
  "Magma interactive mode (using comint)
\\<magma-comint-interactive-mode-map>"
  (setq comint-process-echoes t)
  ;; This doesn't work because magma outputs the prompting "> ", together
  ;; with the input line.
  (setq comint-use-prompt-regexp t)
  (setq comint-prompt-read-only t)
  (setq comint-prompt-regexp magma-prompt-regexp)
  (compilation-shell-minor-mode 1)
  (add-to-list
   'compilation-error-regexp-alist
   '("^In file \"\\(.*?\\)\", line \\([0-9]+\\), column \\([0-9]+\\):$"
     1 2 3 2 1)
   )
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(magma-interactive-font-lock-keywords nil nil))
  )  

(define-derived-mode magma-term-interactive-mode
  term-mode
  "Magma-Interactive"
  "Magma interactive mode (using term)
\\<magma-term-interactive-mode-map>"
  (setq term-scroll-to-bottom-on-output t)
  (make-local-variable 'font-lock-defaults)
  (compilation-shell-minor-mode 1)
  (add-to-list
   'compilation-error-regexp-alist
   '("^In file \"\\(.*?\\)\", line \\([0-9]+\\), column \\([0-9]+\\):$"
     1 2 3 2 1)
   )
  (setq font-lock-defaults '(magma-interactive-font-lock-keywords nil nil))
  )  


(defun magma-init-with-comint ()
  (defalias 'magma-interactive-mode 'magma-comint-interactive-mode)
  (defalias 'magma-run 'magma-comint-run)
  (defalias 'magma-int 'magma-comint-int)
  (defalias 'magma-kill 'magma-comint-kill)
  (defalias 'magma-send 'magma-comint-send)
  (defalias 'magma-help-word-text 'magma-comint-help-word)
  )

(defun magma-init-with-term ()
  (defalias 'magma-interactive-mode 'magma-term-interactive-mode)
  (defalias 'magma-run 'magma-term-run)
  (defalias 'magma-int 'magma-term-int)
  (defalias 'magma-kill 'magma-term-kill)
  (defalias 'magma-send 'magma-term-send)
  (defalias 'magma-help-word-text 'magma-term-help-word)
  )





;; Restent à définir:
;; fun magma-broadcast-kill (?i)
;; fun magma-broadcast-int (?i)
;; fun magma-broadcast-eval-region (start end)
;; fun magma-broadcast-eval-line
;; fun magma-broadcast-eval-paragraph
;; fun magma-broadcast-eval-next-statement
;; fun magma-eval-function (?i)
;; fun magma-broadcast-eval-function
;; fun magma-broadcast-eval
;; fun magma-broadcast-eval-until
;; fun magma-broadcast-broadcast-eval-buffer
;; fun magma-show-word (?i)
;; fun magma-broadcast-show-word
;; fun magma-help-word (?browser)
;; fun magma-help-word-term (topic)
;; fun magma-help-word-browser (topic)
;; var magma-interactive-font-lock-keywords

(provide 'magma-interactive)

