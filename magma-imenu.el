;;; magma-imenu.el --- Support for imenu. ;

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

(require 'imenu)

(defvar magma-defun-regexp "^\\(function\\|procedure\\|intrinsics\\)[[:space:]]+\\(\\sw+\\)[[:space:]]*(")

(setq magma-imenu-generic-expression
      (list (list nil magma-defun-regexp 2)))

(provide 'magma-imenu)
