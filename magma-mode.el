;;; magma-mode.el --- Magma mode for GNU Emacs. ;

;; Copyright (C) 2007-2013 Luk Bettale
;;               2013-2013 Thibaut Verron
;; Licensed under the GNU General Public License.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.


;;; Code:

(provide 'magma-mode)

(defgroup magma nil "Major mode for editting magma-code")

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
    (define-key map "\C-c\C-i" 'magma-int)
    (define-key map "\C-c\C-k" 'magma-kill)
    (define-key map "\C-c\C-x" 'magma-send)
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

(require 'magma-electric-newline)
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

;;; magma-mode.el ends here
