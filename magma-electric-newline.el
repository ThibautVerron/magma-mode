(provide 'magma-electric-newline)

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


(defun magma-not-in-comment-p ()
  "Returns true only if we are not in a magma comment"
  (let ((lit (magma-in-literal)))
    (and (not (eq lit 'c))
	 (not (eq lit 'c++))
	 )
    )
  )

(defun looking-at-end-of-line (&optional endchar)
  "Returns t only is the point is at the end of a line."
  (looking-at (concat endchar "[[:space:]]*$"))
  )

(defun magma-newline-when-in-string ()
  "Inserts a newline in a magma string"
  (progn
    (insert "\"")
    (newline-and-indent)
    (insert "cat \"")
    (if (looking-at-end-of-line)
	(progn 
          (insert "\"")
          (backward-char 1)
          )
      )
    )
  )

(defun magma-special-newline-when-in-string ()
  "Inserts a newline in a magma string, both on display and in the string"
  (progn
    (insert "\\n")
    (magma-newline-when-in-string)
    )
  )

(defun magma-special-newline-when-in-cpp-comment ()
  "Inserts a newline in a C++-like comment"
  (progn
    (newline-and-indent)
    (insert "// ")
    )
  )

(defun magma-newline-when-in-c-comment ()
  "Insert a newline in a C-like comment, preserving the comment
  structure if we're not at the end of line"
  (if (looking-at "[[:space:]]*[^[:space:]].*$")
      (magma-special-newline-when-in-cpp-comment)
    (newline-and-indent))
  )

(defun magma-newline-when-in-cpp-comment ()
  "Inserts a newline in a C++-like comment"
  (let ((col
	 (save-excursion
	   (search-backward "/*")
	   (forward-char 2)
	   (if (looking-at-end-of-line)
	       0
             (+ (current-column) 1)
	     )
	   )
	 )
	)
    (newline)
    (indent-to-column col)
    )
  )

(defun magma-newline-and-indent ()
  "Inserts a newline depending on where the point is"
  (interactive)
  (cl-case (magma-in-literal)
    ('string (magma-newline-when-in-string))
    ('c (magma-newline-when-in-c-comment))
    ('c++ (magma-newline-when-in-cpp-comment))
    (t (newline-and-indent))
    )
  )

(defun magma-special-newline-and-indent ()
  "Inserts a special newline depending on where the point is"
  (interactive)
  (cl-case (magma-in-literal)
    ('string (magma-special-newline-when-in-string))
    ('c++ (magma-special-newline-when-in-cpp-comment))
    (t (magma-newline-and-indent))
    )
  )


(defcustom magma-use-electric-newline nil
  "If non nil, C-j and C-c C-j have special behavior in strings and comments"
  :group 'magma
  :type 'boolean
 )

(if magma-use-electric-newline
    (progn
      (define-key magma-mode-map (kbd "RET") 'magma-newline-and-indent)
      (define-key magma-mode-map (kbd "C-j") 'magma-newline-and-indent)
      (define-key magma-mode-map (kbd "C-c C-j")
        'magma-special-newline-and-indent)
      (define-key magma-mode-map (kbd "C-RET")
        'magma-special-newline-and-indent)
      )
  (progn
    (define-key magma-mode-map (kbd "RET") nil)
    (define-key magma-mode-map (kbd "C-j") nil)
    (define-key magma-mode-map (kbd "C-c C-j") nil)
    (define-key magma-mode-map (kbd "C-RET") nil)))
