;;; magma-mode.el --- Magma mode for GNU Emacs. ;

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

(defgroup magma nil "Major mode for editting magma-code")

(require 'cl)
(require 'dash)
(require 'f)
(require 'thingatpt)

(defconst magma-path (f-dirname (f-this-file)) "magma-mode install folder")
;;(add-to-list 'load-path magma-path)

(defcustom magma-default-directory "~/"
  "Default work directory for magma"
  :group 'magma
  :type 'string)

(defvar magma--debug-level 0 "Echo basic debug information?")

(defun magma--debug-message (str)
  (when (>= magma--debug-level 1) (message str)))

(defun magma--debug2-message (str)
  (when (>= magma--debug-level 2) (message str)))

(defvar magma-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c :") 'magma-send-expression)
    (define-key map (kbd "C-c C-e") 'magma-eval)
    (define-key map (kbd "C-c C-f") 'magma-eval-defun)
    (define-key map (kbd "C-c C-u") 'magma-eval-until)
    (define-key map (kbd "C-c C-l") 'magma-eval-line)
    (define-key map (kbd "C-c C-p") 'magma-eval-paragraph)
    (define-key map (kbd "C-c C-b") 'magma-eval-buffer)
    (define-key map (kbd "C-c C-n") 'magma-set-working-buffer)
    (define-key map (kbd "C-c C-o")
      'magma-switch-to-interactive-buffer-same-frame)
    (define-key map (kbd "C-c o") 'magma-switch-to-interactive-buffer)
    (define-key map (kbd "C-c C-a") 'magma-restart)
    (define-key map (kbd "C-c C-i") 'magma-int)
    (define-key map (kbd "C-c C-k") 'magma-kill)
    (define-key map (kbd "C-c C-h") 'magma-help-word)
    (define-key map (kbd "C-c C-w") 'magma-show-word)
    (define-key map [remap forward-paragraph] 'magma-end-of-expr)
    (define-key map [remap backward-paragraph] 'magma-previous-expr)
    map)
  "Keymap for magma-mode"
  )

(defvar magma-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?/  ". 142"  st) 
    (modify-syntax-entry ?*  ". 23b"  st) 
    (modify-syntax-entry ?\n  ">"     st) 
    (modify-syntax-entry ?\r  ">"     st) 
    (modify-syntax-entry ?+  "."      st)
    (modify-syntax-entry ?-  "."      st)
    (modify-syntax-entry ?=  "."      st)
    (modify-syntax-entry ?!  "."      st)
    (modify-syntax-entry ?<  "("      st) 
    (modify-syntax-entry ?>  ")"      st) 
    (modify-syntax-entry ?&  "."      st)
    (modify-syntax-entry ?#  "."      st)
    (modify-syntax-entry ?`  "."      st)
    (modify-syntax-entry ?|  "."      st)
    (modify-syntax-entry ?_  "w"      st)
    (modify-syntax-entry ?%  "_"      st)
    (modify-syntax-entry ?$  "_"      st)
    st)
  "*Syntax table used while in `magma-mode'.")

(require 'magma-font-lock)
(require 'magma-smie)
(require 'magma-extra)
(require 'magma-interactive)

(define-derived-mode magma-mode
  prog-mode
  "Magma"
  "Magma mode"
  (use-local-map magma-mode-map)
  (set-syntax-table magma-mode-syntax-table)
  (make-local-variable 'comment-start)
  (setq comment-start "/* ")
  (make-local-variable 'comment-end)
  (setq comment-end " */")
  (setq imenu-generic-expression magma-imenu-generic-expression)
  (smie-setup
   magma-smie-grammar
   #'magma-smie-rules
   :forward-token #'magma-smie-forward-token
   :backward-token #'magma-smie-backward-token)
  
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(magma-font-lock-keywords nil nil ((?_ . "w"))))

  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'smie-indent-line)

  (magma-interactive-init))
  

(provide 'magma-mode)


;;; magma-mode.el ends here
