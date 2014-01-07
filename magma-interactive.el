(provide 'magma-interactive)

(defvar magma-interactive-program "magma"
  "*Program to be launched to use magma (usually magma)")

(defvar magma-default-directory "~/magma"
  "Default work directory for magma")

(add-to-list 'tramp-remote-path "~/bin")

(defvar magma-working-buffer-number 0
  "Should this buffer send instructions to a different magma buffer")

(defvar magma-active-buffers-list '()
  "*List of active magma buffers.")

(defvar magma-interactive-buffer-name "magma"
  "*Name of the buffer to be used for using magma
  interactively (will be surrounded by `*')")

(defvar magma-interactive-arguments '()
  "Commandline arguments to pass to magma")

(defvar magma-interactive-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    ;; example definition
    (define-key map "\t" 'completion-at-point)
    map)
  "Keymap for magma-interactive-mode")
  
(defvar magma-prompt-regexp "^[^ ]*> "
  "Regexp matching the magma prompt")

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

(defun magma-run (&optional i)
  "Run an inferior instance of magma inside Emacs."
  ;;(interactive)
  (let* ((default-directory
          (if
              (or
               (not (file-remote-p default-directory)) 
               (tramp-sh-handle-executable-find magma-interactive-program))
              default-directory
            magma-default-directory
            ))
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
    (magma-interactive-mode)
  ))

(defun magma-int (&optional i)
  "Interrupt the magma process in buffer i"
  (interactive "P")
  (set-buffer (magma-get-buffer i))
  (comint-interrupt-subjob)
  )

(defun magma-kill (&optional i)
  "Kill the magma process in buffer i"
  (interactive "P")
  (set-buffer (magma-get-buffer i))
  (comint-kill-subjob)
  )

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

;; For all the evaluation functions, the universal argument causes the output
;; to be pasted under the input as a comment. (TODO)
;;
;; With this, we aren't compatible with the settings for
;; magma-mode.el. The reason is that I believe the magma process
;; should only be set using magma-working-buffer-number, and maybe
;; also changed with file-local variables. The only issue I can see
;; with this is that it makes broadcasting a signal rather
;; tedious. The key bindings also need to be rethought.

(defun magma-send (expr &optional ins)
  "Send the expression expr to the magma buffer for evaluation."
  (interactive "MExpression?\nP")
  (let ((command (concat expr "\n")))
    (if ins
        (comint-redirect-send-command-to-process command (current-buffer) (magma-get-buffer))
      (comint-send-string (magma-get-buffer) command))
    )
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
  (magma-eval-region (point-min) (point-max)))

(defvar magma-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-e" 'magma-eval)
    (define-key map "\C-c\C-u" 'magma-eval-until)
    (define-key map "\C-c\C-l" 'magma-eval-line)
    (define-key map "\C-c\C-p" 'magma-eval-paragraph)
    (define-key map "\C-c\C-b" 'magma-eval-buffer)
    (define-key map "\C-c\C-n" 'magma-set-working-buffer)
    (define-key map "\C-c\C-o"
      'magma-switch-to-interactive-buffer-same-frame)
    (define-key map "\C-co" 'magma-switch-to-interactive-buffer)
    (define-key map "\C-c\C-a" 'magma-restart)
    (define-key map "\C-c\C-i" 'magma-restart)
    (define-key map "\C-c\C-k" 'magma-kill)
    map)
  "Keymap for magma-mode"
  )

(define-derived-mode magma-mode
  magma-mode
  "Magma"
  "Magma mode"
  (use-local-map magma-mode-map)
  )

(define-derived-mode magma-interactive-mode
  comint-mode
  "Magma-Interactive"
  "Magma interactive mode"
  (setq comint-prompt-regexp magma-prompt-regexp)
  (compilation-shell-minor-mode)
  (run-hooks 'magma-interactive-mode-hook)
  )                                 

(defun magma-interactive-initialize ()
  "Various buffer-local settings"
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t)
  (add-to-list
   'compilation-error-regexp-alist
   '("^In file \"\\(.*?\\)\", line \\([0-9]+\\), column \\([0-9]+\\):$" 1 2 3 2 1)
   )
  )

(add-hook 'magma-interactive-mode-hook 'magma-interactive-initialize)

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

