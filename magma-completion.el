;;; magma-completion.el --- Code completion for magma. ;

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

(require 'magma-smie)
(require 'magma-scan)

(declare-function magma-mode "magma-mode.el")

(defvar magma-completion-table-file (f-join magma-path "data/magma_symbols.txt"))

(defvar magma-completion-table-base
  (magma-scan-completion-file magma-completion-table-file))

(defvar-local magma-completion-table nil)
  
(defun magma-find-completions-at-point ()
  (let* ((bounds (bounds-of-thing-at-point 'word))
         (start (car bounds))
         (end (cdr bounds)))
    (and bounds (list start end magma-completion-table :exclusive 'no))))

(defun magma-init-completion ()
  "Function run at mode initialisation, activating the completion and defining
 its initial dictionary."
  (interactive)
  (setq magma-completion-table magma-completion-table-base)
  (make-local-variable 'completion-at-point-functions)
  (setq completion-at-point-functions (list 'magma-find-completions-at-point))
  )

(defun magma-interactive-add-to-completion-table (str)
  "Parse the string str, and extract new symbols to add to the completion table"
  (magma--debug-message "Scanning input for completion candidates...")
  (magma--debug-message (format "Input : %s" str))
  (setq magma-completion-table
        (let ((prev-table magma-completion-table))
          (save-excursion
           (with-temp-buffer
             (let ((magma-mode-hook nil))
               (magma-mode))
             (setq magma-completion-table prev-table)
             (insert str)
             (magma-scan t)
             magma-completion-table)))))


(defun magma-interactive-init-completion ()
  (magma-init-completion)
  (magma-interactive-rebuild-completion-table)
  (add-hook 'comint-input-filter-functions
            'magma-interactive-add-to-completion-table nil t))


(defun magma-editor-init-completion ()
  (magma-init-completion)
  (magma-editor-rebuild-completion-table)
  )

(add-hook 'magma-comint-interactive-mode-hook 'magma-interactive-init-completion)

(add-hook 'magma-mode-hook 'magma-editor-init-completion)


(defun magma-editor-rebuild-completion-table ()
  (interactive)
  (magma--debug-message "Rebuilding the completion table...")
  (ignore (magma-scan)))


(defun magma-interactive-rebuild-completion-table ()
  (interactive)
  nil)

(defun magma-completion-at-point ()
  (interactive)
  (magma-editor-rebuild-completion-table)
  ;; Fixme: maybe rebuild only if called twice, or something...
  (completion-at-point))

;; Extra functions for the snippets

(defun magma-filename-p (str)
  (string-match-p "\\.m$" str))

(provide 'magma-completion)

;;; magma-completion.el ends here
