;;; magma-mode.el --- Magma mode for Emacs

;; Copyright (C) 2007-2014 Luk Bettale
;;               2013-2014 Thibaut Verron
;; Licensed under the GNU General Public License.

;; Package-requires: ((cl-lib "0.3") (dash "2.6.0") (f "0.17.1"))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;;; Commentary:

;; Basic setup:
;;
;; Ensure that the installation directory is in your `load-path' (it
;; is done for you if installing from melpa), and add the following
;; line to your emacs init file:
;;
;;     (require 'magma-mode)
;;
;; Additionally, if you want to load the mode automatically with some file extensions, you can add the following to your init file:
;;
;;     (setq auto-mode-alist
;;     (append '(("\\.mgm$\\|\\.m$" . magma-mode))
;;             auto-mode-alist))
;;
;; Some features are available in `magma-extra.el'. They are disabled
;; because they are more intrusive than the others. Feel free to
;; browse the customize interface to enable some of them!
;;
;; Some support for `hs-minor-mode', `imenu' and `smart-parens' is
;; also provided.
;;
;; If you are using `yasnippet', you can enable some snippets for
;; `magma-mode' by adding the following to your init file.
;;
;;     (require 'magma-snippet)
;;
;; At the moment, these snippets include basic syntactic constructs
;; (if, while, for, etc.) and load (with file name completion). More
;; will be added in the future.
;;
;; The complete documentation is available in README.org or on
;; https://github.com/ThibautVerron/magma-mode

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'f)
(require 'thingatpt)

(defgroup magma nil "Major mode for editting magma-code")

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
    (define-key map (kbd "C-c ]") 'magma-close-block) ;; Similar to AUCTeX
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

;;;###autoload
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

  (set (make-local-variable 'indent-line-function)
       'smie-indent-line)

  (magma--apply-electric-newline-setting)
  
  (magma-interactive-init))
  

(provide 'magma-mode)


;;; magma-mode.el ends here
