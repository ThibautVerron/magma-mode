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
  "Inserts a newline in a magma string"
  (progn
    (insert "\"")
    (insert "cat \"")
    (forward-char -5)
    (magma-newline-and-indent)
    (if (magma-looking-at-end-of-line)
	(progn 
          (insert "\"")
          (backward-char 1)))))

(defun magma-special-newline-when-in-string ()
  "Inserts a newline in a magma string, both on display and in the string"
  (progn
    (insert "\\n")
    (magma-newline-when-in-string)))

(defun magma-special-newline-when-in-c-comment ()
  "Inserts a newline in a C++-like comment"
  (progn
    (magma-newline-and-indent)
    (insert "// ")))

(defun magma-newline-when-in-c-comment ()
  "Insert a newline in a C-like comment, preserving the comment
  structure if we're not at the end of line"
  (if (looking-at "[[:space:]]*[^[:space:]\n].*$")
      (magma-special-newline-when-in-c-comment)
    (magma-newline-and-indent)))

(defun magma-newline-when-in-cpp-comment ()
  "Inserts a newline in a C++-like comment"
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
  "Like newline-and-indent, but without deleting the trailing spaces"
  (interactive)
  (newline)
  (magma-indent-line))

(defun magma-insert-newline ()
  "Inserts a newline depending on where the point is"
  (interactive)
  (cl-case (car (magma-in-literal))
    ('string (magma-newline-when-in-string))
    ('c (magma-newline-when-in-c-comment))
    ('c++ (magma-newline-when-in-cpp-comment))
    (t (magma-newline-and-indent))))

(defun magma-insert-special-newline ()
  "Inserts a special newline depending on where the point is"
  (interactive)
  (cl-case (car (magma-in-literal))
    ('string (magma-special-newline-when-in-string))
    ('c (magma-special-newline-when-in-c-comment))
    ('c++ (magma-newline-when-in-cpp-comment))
    (t (magma-newline-and-indent))))


(defun magma-set-electric-newline (symbol value)
  (set-default symbol value)
  (magma--apply-electric-newline-setting))

(defun magma--apply-electric-newline-setting ()
  (if magma-use-electric-newline
      (progn
        (define-key magma-mode-map (kbd "RET") 'magma-insert-newline)
        (define-key magma-mode-map (kbd "C-j") 'magma-insert-newline)
        (define-key magma-mode-map (kbd "C-c C-j")
          'magma-insert-special-newline)
        (define-key magma-mode-map (kbd "C-<return>")
          'magma-insert-special-newline))
    (progn
      (define-key magma-mode-map (kbd "RET") nil)
      (define-key magma-mode-map (kbd "C-j") nil)
      (define-key magma-mode-map (kbd "C-c C-j") nil)
      (define-key magma-mode-map (kbd "C-RET") nil))))

(defun magma-toggle-electric-newline ()
  (interactive)
  (setq magma-use-electric-newline (not magma-use-electric-newline))
  (magma--apply-electric-newline-setting))

(defcustom magma-use-electric-newline nil
  "If non nil, C-j and C-c C-j have special behavior in strings and comments"
  :group 'magma
  :set 'magma-set-electric-newline
  :type 'boolean)


;; Smartparens
;;;;;;;;;;;;;;

(defun magma-smartparens-gt-in-an-arrow (id beg end)
  "Test ensuring that \"->\" does not mark the end of the
  surrounding \"<...>\" pair."
  (save-excursion
    (goto-char (- end 1))
    (looking-back "-")))

(eval-after-load "smartparens.el"
  '(sp-with-modes '(magma-mode magma-comint-interactive-mode magma-term-interactive-mode)
     (sp-local-pair "<" ">" :skip-match 'magma-smartparens-gt-in-an-arrow :actions '(insert wrap navigate))
     (sp-local-pair "`" nil :actions nil)))


;; File header
;;;;;;;;;;;;;;

(defcustom magma-file-header nil
  "If non-nil, magma will maintain a file header for the magma
  files. This variable should then be either `default', in which case
  we use the default header (see the documentation for
  `magma-update-header-default'), or the name of a function which
  will create and update this header.

  This variable can always be overridden as a file-local variable."
  :group 'magma)

(defun magma-update-header-default ()
  "Default header for magma files. The first line is the date of
  creation, the second the date of last modification, and the
  third a hash of the rest of the buffer. This function is
  intended to be user together with `yasnippet' and
  `magma-initial-file' set to `t', in order to create the initial content.

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
        (insert (format "// Hash: %s\n" (secure-hash 'md5 (current-buffer) start (point-max))))))))

(defun magma-update-header ()
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
  "Contents to insert in a new magma file. This can be either
  `default', in which case we insert a skeleton of the header
  described in `magma-update-header-default', or a function name which
  is then evaluated.

Based on `auto-insert'"
  :group 'magma)

(defun magma-initial-file-contents-default ()
  "Insert a skeleton of the header described in `magma-update-header-default'"
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
  (when (and magma-initial-file
             buffer-file-name
             ;; Make sure auto-insert has not been already called
             (= (point-max) 1))
    (if (eq magma-initial-file 'default)
        (magma-initial-file-contents-default)
      (funcall magma-initial-file))))

(add-hook 'magma-mode-hook 'auto-insert)

(define-auto-insert 'magma-mode 'magma-initial-file-contents)



(provide 'magma-extra)

;;; magma-extra.el ends here
