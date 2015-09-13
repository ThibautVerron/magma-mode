;;; magma-mode.el --- Magma mode for Emacs

;; Copyright (C) 2007-2014 Luk Bettale
;;               2013-2014 Thibaut Verron
;; Licensed under the GNU General Public License.

;; Package-requires: ((cl-lib "0.3") (dash "2.6.0") (f "0.17.1"))
;; URL: https://github.com/ThibautVerron/magma-mode

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
;; Additionally, if you want to load the mode automatically with some
;; file extensions, you can add the following to your init file:
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
;;     (require 'magma-snippets)
;;
;; At the moment, these snippets include basic syntactic constructs
;; (if, while, for, etc.) and load (with file name completion). More
;; will be added in the future.
;;
;; The complete documentation is available on
;; https://github.com/ThibautVerron/magma-mode
;;
;; Bug reports and suggestions are welcome, please use the github
;; issue tracker.

;;; Code:

(require 'magma-vars)
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
  (setq-local syntax-propertize-function magma-syntax-propertize-function)
  (setq-local comment-start "/* ")
  (setq-local comment-end " */")
  (setq imenu-generic-expression magma-imenu-generic-expression)
  (smie-setup
   magma-smie-grammar
   #'magma-smie-rules
   :forward-token #'magma-smie-forward-token
   :backward-token #'magma-smie-backward-token)
  
  (setq-local font-lock-defaults
              '(magma-font-lock-keywords nil nil ((?_ . "w"))))

  (setq-local indent-line-function 'smie-indent-line)

  ;(magma--apply-electric-newline-setting) 
  
  (magma-interactive-init))
  

(provide 'magma-mode)


;;; magma-mode.el ends here
