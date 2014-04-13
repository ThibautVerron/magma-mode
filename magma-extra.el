;;; magma-extra.el --- 

;; Copyright (C) 2014  Thibaut VERRON

;; Author: Thibaut VERRON <verron@ilithye.calsci.lip6.fr>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

;;;;;;;;;;;;;;;;;;;;
;; Support for imenu
;;;;;;;;;;;;;;;;;;;;

(require 'imenu)

(defvar magma-defun-regexp "^\\(function\\|procedure\\|intrinsics\\)[[:space:]]+\\(\\sw+\\)[[:space:]]*(")

(setq magma-imenu-generic-expression
      (list (list nil magma-defun-regexp 2)))


;;;;;;;;;;;;;;;;;;;;;;;
;; Support for hideshow
;;;;;;;;;;;;;;;;;;;;;;;

(push '(magma-mode "\\(function\\|procedure\\|for\\|while\\|try\\|if\\|case\\)"
                   "end \\(function\\|procedure\\|for\\|while\\|try\\|if\\|case\\);" "/[/*]" nil nil)
      hs-special-modes-alist)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Electric editting facilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;; Newline
;;;;;;;;;;;;

(defun magma-newline-when-in-string ()
  "Inserts a newline in a magma string"
  (progn
    (insert "\"")
    (magma-newline-and-indent)
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

(defun magma-special-newline-when-in-c-comment ()
  "Inserts a newline in a C++-like comment"
  (progn
    (magma-newline-and-indent)
    (insert "// ")
    )
  )

(defun magma-newline-when-in-c-comment ()
  "Insert a newline in a C-like comment, preserving the comment
  structure if we're not at the end of line"
  (if (looking-at "[[:space:]]*[^[:space:]\n].*$")
      (magma-special-newline-when-in-c-comment)
    (magma-newline-and-indent))
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
  "Like newline-and-indent, but without deleting the trailing spaces"
  (interactive)
  (newline)
  (magma-indent-line)
  )

(defun magma-insert-newline ()
  "Inserts a newline depending on where the point is"
  (interactive)
  (cl-case (magma-in-literal)
    ('string (magma-newline-when-in-string))
    ('c (magma-newline-when-in-c-comment))
    ('c++ (magma-newline-when-in-cpp-comment))
    (t (magma-newline-and-indent))
    )
  )

(defun magma-insert-special-newline ()
  "Inserts a special newline depending on where the point is"
  (interactive)
  (cl-case (magma-in-literal)
    ('string (magma-special-newline-when-in-string))
    ('c (magma-special-newline-when-in-c-comment))
    ('c++ (magma-newline-when-in-cpp-comment))
    (t (magma-newline-and-indent))
    )
  )


(defcustom magma-use-electric-newline nil
  "If non nil, C-j and C-c C-j have special behavior in strings and comments"
  :group 'magma
  :type 'boolean
  )

(defun magma--apply-electric-newline-setting ()
  (if magma-use-electric-newline
      (progn
        (define-key magma-mode-map (kbd "RET") 'magma-insert-newline)
        (define-key magma-mode-map (kbd "C-j") 'magma-insert-newline)
        (define-key magma-mode-map (kbd "C-c C-j")
          'magma-insert-special-newline)
        (define-key magma-mode-map (kbd "C-<return>")
          'magma-insert-special-newline)
        )
    (progn
      (define-key magma-mode-map (kbd "RET") nil)
      (define-key magma-mode-map (kbd "C-j") nil)
      (define-key magma-mode-map (kbd "C-c C-j") nil)
      (define-key magma-mode-map (kbd "C-RET") nil))))

(defun magma-toggle-electric-newline ()
  (interactive)
  (setq magma-use-electric-newline (not magma-use-electric-newline))
  (magma--apply-electric-newline-setting))




(provide 'magma-extra)

;;; magma-extra.el ends here
