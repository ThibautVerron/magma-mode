(provide 'magma-mode)

(require 'f)

(defvar magma-path (f-dirname (f-this-file)))


(defun magma-in-literal ()
  "Return the type of literal point is in, if any.
The return value is `c' if in a C-style comment, `c++' if in a
C++ style comment, `string' if in a string literal, `intrinsic'
if in an intrinsic description or nil if somewhere else."
  (let ((state (parse-partial-sexp (point-min) (point))))
    (cond
     ((and
       (= (elt state 0) 1)
       (= (char-after (elt state 1)) ?{)
       (save-match-data
         (looking-back
          (concat
           "\\<intrinsic\\>[^;]*"
           (regexp-quote
            (buffer-substring-no-properties
             (elt state 1) (point)))))))
      'intrinsic)
     ((elt state 3) 'string)
     ((elt state 4) (if (elt state 7) 'c++ 'c))
     (t nil))))


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



(define-key magma-mode-map (kbd "RET") 'magma-newline-and-indent)
(define-key magma-mode-map (kbd "C-j") 'magma-newline-and-indent)
(define-key magma-mode-map (kbd "C-c C-j") 'magma-special-newline-and-indent)
(define-key magma-mode-map (kbd "C-RET") 'magma-special-newline-and-indent)

(defun magma-not-in-comment-p ()
  "Returns true only if we are not in a magma comment"
  (let ((lit (magma-in-literal)))
    (and (not (eq lit 'c))
	 (not (eq lit 'c++))
	 )
    )
  )

(defun magma-eval-buffer-with-load ()
  "Commande perso : évalue le buffer en envoyant \"load fichier\" si possible"
  (interactive)
  (let ((load-expr
	 (save-excursion
	   (goto-line 2)
	   (forward-char 3)
	   (looking-at "\\(load \".*\";\\)")
	   (match-string 1)
	   )
	 ))
    (if load-expr
	(progn
	  (save-buffer)
	  (let ((cur-buffer (current-buffer)))
	    (magma-switch-to-interactive-buffer)
            (end-of-buffer)
	    (insert load-expr)
	    (term-send-input)
	    )
	  )
      (magma-eval-buffer))
    )
  )

(defun magma-eval-buffer-with-load ()
  "Comme la précédente, mais avec comint"
  (interactive)
  (let ((load-expr
	 (save-excursion
	   (goto-line 2)
	   (forward-char 3)
	   (looking-at "\\(load \".*\";\\)")
	   (match-string 1)
	   )
	 ))
    (if load-expr
        (magma-send load-expr)
      (magma-eval-buffer))
    )
  )

(define-key magma-mode-map (kbd "C-c C-b") 'magma-eval-buffer-with-load)


;; Previous values were weird

(defvar magma-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?/  ". 142"  st) ;; Fixed
    (modify-syntax-entry ?*  ". 23b"  st) ;; Fixed
    (modify-syntax-entry ?\n  ">"     st) ;; Fixed
    (modify-syntax-entry ?\r  ">"     st) ;; Fixed
    (modify-syntax-entry ?+  "."      st)
    (modify-syntax-entry ?-  "."      st)
    (modify-syntax-entry ?=  "."      st)
    (modify-syntax-entry ?!  "."      st)
    (modify-syntax-entry ?<  "("      st) ;; Real change
    (modify-syntax-entry ?>  ")"      st) ;; Real change
    (modify-syntax-entry ?&  "."      st)
    (modify-syntax-entry ?#  "."      st)
    (modify-syntax-entry ?`  "."      st)
    (modify-syntax-entry ?|  "."      st)
    (modify-syntax-entry ?_  "w"      st)
    (modify-syntax-entry ?%  "_"      st)
    (modify-syntax-entry ?$  "_"      st)
    st)
  "*Syntax table used while in `magma-mode'.")

;; Also that didn't make any sense. Or did it? Yes it did. Too lazy to
;; undo now, but make sure not to add that to the release.


;; Here an attempt of fix for the indentation of stuff before end something

(require 'magma-fancy-electric)
(require 'magma-font-lock)
(require 'magma-smie)
(require 'magma-interactive)

(define-derived-mode magma-mode
  prog-mode
  "Magma"
  "Magma mode"
  (use-local-map magma-mode-map)
  (set-syntax-table magma-mode-syntax-table)
  (make-local-variable 'comment-start)
  (setq comment-start "/* ")
  ;; (make-local-variable 'comment-padding)
  ;; (setq comment-padding " * ")
  (make-local-variable 'comment-end)
  (setq comment-end " */")
  (smie-setup
   magma-smie-grammar
   #'magma-smie-rules
   :forward-token #'magma-smie-forward-token
   :backward-token #'magma-smie-backward-token)
  
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(magma-font-lock-keywords nil nil ((?_ . "w"))))

  )

(defun magma-mode-uninstall ()
  (interactive)
  "Unbinds some of the variables defined by magma-mode. Intended mainly
for testing."
  (dolist (var
           '(magma-smie-grammar
             magma-smie-tokens-regexp
             magma-smie-end-tokens-regexp
             magma-smie-operators-regexp
             magma-smie-special1-regexp
             magma-smie-special2-regexp))
           (makunbound var))
    )
