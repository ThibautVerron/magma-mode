;;; magma-extra.el --- Extra features for magma-mode
;;; magma-extra.el ---

;; Copyright (C) 2014  Thibaut VERRON

;; Author: Thibaut VERRON <verron@ilithye.calsci.lip6.fr>
;; Keywords:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'magma-vars)
(require 'magma-smie)

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

;; Newline
;;;;;;;;;;;;

(defun magma-newline-when-in-string ()
  "Insert a newline in a magma string."
  (insert "\"cat \"")
  (forward-char -5)
  (magma-newline-and-indent)
  (forward-char 5))

(defun magma-special-newline-when-in-string ()
  "Insert a newline in a magma string, both on display and in the string."
  (insert "\\n")
  (magma-newline-when-in-string))

(defun magma-special-newline-when-in-c-comment ()
  "Insert a newline in a C++-like comment."
  (magma-newline-and-indent)
  (insert "// "))

(defun magma-newline-when-in-c-comment ()
  "Insert a newline in a C-like comment.

If we are not at the end of the line, preserve the comment structure."
  (if (looking-at "[[:space:]]*[^[:space:]\n].*$")
      (magma-special-newline-when-in-c-comment)
    (magma-newline-and-indent)))

(defun magma-newline-when-in-cpp-comment ()
  "Insert a newline in a C++-like comment."
  (let ((col
	 (save-excursion
	   (search-backward "/*")
	   (forward-char 2)
	   (if (magma-looking-at-end-of-line)
	       0
             (+ (current-column) 1)))))
    (newline)
    (indent-to-column col)))

(defun magma-newline-and-indent ()
  "Like ‘newline-and-indent’, but without deleting the trailing spaces."
  (interactive)
  (newline)
  (magma-indent-line))

(defun magma-insert-newline ()
  "Insert a newline depending on where the point is."
  (interactive)
  (cl-case (car (magma-in-literal))
    ('string (magma-newline-when-in-string))
    ('c (magma-newline-when-in-c-comment))
    ('c++ (magma-newline-when-in-cpp-comment))
    (t (magma-newline-and-indent))))

(defun magma-insert-special-newline ()
  "Insert a special newline depending on where the point is."
  (interactive)
  (cl-case (car (magma-in-literal))
    ('string (magma-special-newline-when-in-string))
    ('c (magma-special-newline-when-in-c-comment))
    ('c++ (magma-newline-when-in-cpp-comment))
    (t (magma-newline-and-indent))))


;; Smartparens
;;;;;;;;;;;;;;

(defun magma-smartparens-gt-in-an-arrow (id beg end)
  "Check whether > is part of an arrow.

ID, BEG and END are arguments for the `smartparens' interface and are ignored."
  (save-excursion
    (goto-char (- end 1))
    (looking-back "-" nil)))

(declare-function sp-with-modes "ext:smartparens.el" t t)
(declare-function sp-local-pair "ext:smartparens.el" t t)

(eval ;; Hide this form from the byte-compiler
 '(eval-after-load 'smartparens
    '(sp-with-modes '(magma-mode
                      magma-comint-interactive-mode
                      magma-term-interactive-mode)
       (sp-local-pair "<" ">"
                      :actions '(insert wrap autoskip)
                      :skip-match 'magma-smartparens-gt-in-an-arrow)
       (sp-local-pair "`" nil :actions '()))))
 


;; File header
;;;;;;;;;;;;;;

(defcustom magma-file-header nil
  "File header for magma source files.

If non-nil, magma will maintain a file header for the magma
  files.  This variable should then be either `default', in which case
  we use the default header (see the documentation for
  `magma-update-header-default'), or the name of a function which
  will create and update this header.

  This variable can always be overridden as a file-local variable."
  :group 'magma)

(defun magma-update-header-default ()
  "Update the default header for magma files.

 The first line is the date of
  creation, the second the date of last modification, and the
  third a hash of the rest of the buffer.  This function is
  intended to be user together with `yasnippet' and
  `magma-initial-file' set to t, in order to create the initial content.

  `magma-update-header-default' will only operate if the file
  begins with \"// Created\", in order not to accidentally
  overwrite contents of a file."
  (save-excursion
    (goto-char (point-min))
    (when (looking-at "/[*/] -\\*-") (forward-line 1)) ;; prop-line
    (when (looking-at "// Created:")
      (forward-line 1)
      (when (looking-at "// Last modified:")
        (delete-region (point) (progn (forward-line 1) (point))))
      (insert (format "// Last modified: %s\n" (current-time-string)))
      (when (looking-at "// Hash:")
        (delete-region (point) (progn (forward-line 1) (point))))
      (let ((start
             (save-excursion
               (forward-line 2)
               (point))))
        (insert (format
                 "// Hash: %s\n"
                 (secure-hash 'md5 (current-buffer) start (point-max))))))))

(defun magma-update-header ()
  "Update the header for magma files."
  (when magma-file-header
    (if (eq magma-file-header 'default)
        (magma-update-header-default)
      (funcall magma-file-header))))

(add-hook
 'magma-mode-hook
 (lambda ()
   (add-hook 'write-contents-functions
             'magma-update-header)))

(defcustom magma-initial-file nil
  "Contents to insert in a new magma file.

This can be either
  `default', in which case we insert a skeleton of the header
  described in `magma-update-header-default', or a function name which
  is then evaluated.

Based on function `auto-insert'"
  :group 'magma)

(defun magma-initial-file-contents-default ()
  "Insert a skeleton of the header described in `magma-update-header-default'."
  (insert (format "// Created: %s\n" (current-time-string)))
  (insert "// Last modified:\n")
  (insert "// Hash:\n")
  (insert (format "// load \"%s\";\n"
                  (replace-regexp-in-string
                   "/scpc:.*?:" ""
                   buffer-file-name)))
  (insert "\n")
  (insert (format "ChangeDirectory(\"%s\");"
                  (replace-regexp-in-string
                   "/scpc:.*?:" ""
                   default-directory)))
  (insert "\n"))


(defun magma-initial-file-contents ()
  "Insert the initial file contents if needed."
  (when (and magma-initial-file
             buffer-file-name
             ;; Make sure auto-insert has not been already called
             (= (point-max) 1))
    (if (eq magma-initial-file 'default)
        (magma-initial-file-contents-default)
      (funcall magma-initial-file))))

(define-auto-insert 'magma-mode 'magma-initial-file-contents)

(provide 'magma-extra)

;;; magma-extra.el ends here
