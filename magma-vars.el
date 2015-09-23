;;; magma-vars.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Thibaut Verron

;; Author: Thibaut Verron <thibaut-suse@sherlock-suse>
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

(require 'cl-lib)
(require 'dash)
(require 'f)
(require 'thingatpt)

(defgroup magma nil "Major mode for editting magma-code"
  :group 'Programming)

(defconst magma-path (f-dirname (f-this-file)) "magma-mode install folder")
;;(add-to-list 'load-path magma-path)

(defcustom magma-default-directory nil
  "Default work directory for magma"
  :group 'magma
  :type '(choice string (const nil)))

(defvar magma--debug-level 0 "Echo basic debug information?")

(defun magma--debug-message (str)
  (when (>= magma--debug-level 1) (message str)))

(defun magma--debug2-message (str)
  (when (>= magma--debug-level 2) (message str)))

(defconst magma-mode-map
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
    (define-key map (kbd "C-c h") 'magma-help-word)
    (define-key map (kbd "C-c C-h") 'magma-help-word-browser)
    (define-key map (kbd "C-c C-w") 'magma-show-word)
    (define-key map [remap forward-paragraph] 'magma-end-of-expr)
    (define-key map [remap backward-paragraph] 'magma-previous-expr)
    (define-key map (kbd "C-c ]") 'magma-close-block) ;; Similar to AUCTeX
    map)
  "Keymap for magma-mode"
  )

(defconst magma-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?/  ". 142"  st) 
    (modify-syntax-entry ?*  ". 23b"  st) 
    (modify-syntax-entry ?\n  ">"     st) 
    (modify-syntax-entry ?\r  ">"     st) 
    (modify-syntax-entry ?+  "."      st)
    (modify-syntax-entry ?-  "."      st)
    (modify-syntax-entry ?=  "."      st)
    (modify-syntax-entry ?!  "."      st)
    (modify-syntax-entry ?<  "(>"     st) 
    (modify-syntax-entry ?>  ")<"     st) 
    (modify-syntax-entry ?&  "."      st)
    (modify-syntax-entry ?#  "."      st)
    (modify-syntax-entry ?`  "."      st)
    (modify-syntax-entry ?|  "."      st)
    (modify-syntax-entry ?_  "w"      st)
    (modify-syntax-entry ?%  "_"      st)
    (modify-syntax-entry ?$  "_"      st)
    st)
  "*Syntax table used while in `magma-mode'.")

(defconst magma-syntax-propertize-function 
  (syntax-propertize-rules ("-\\(>\\)" (1 "."))))

;; Helper functions, maybe we should put them in their own file in the future

(defun magma--comment-kill-no-kill-ring (count)
  "Same as comment-kill but without pushing the comments to the kill-ring"
  (let (kill-ring)
    (comment-kill count)))

(provide 'magma-vars)
;;; magma-vars.el ends here
