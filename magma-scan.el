;;; magma-scan.el --- Scan magma input for completion candidates. ;

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

(defvar-local magma-working-directory magma-default-directory)

(defun magma-scan (&optional str)
  "Scans the current magma buffer. Updates the working directory,
  and returns the list of the names of functions defined in the
  buffer, and in files it loads."
  (interactive)
  (magma--debug-message "Scanning the buffer")
  (let*
      ((content (or str (buffer-substring-no-properties (point-min) (point-max))))
       (workdir magma-working-directory)
       (res (with-temp-buffer
              (let ((magma-mode-hook nil))
                (magma-mode))
              (insert content)
              (setq magma-working-directory workdir)
              (goto-char (point-min))
              (newline) ;; Otherwise, comment-kill fails if there is a comment on line 1
              
              ;; Get rid of the comments
              (comment-kill (count-lines (point-min) (point-max)))
              (goto-char (point-min))

              (setq defs nil)
              ;; And scan
              (setq moreLines t)
              
              (while moreLines
                (beginning-of-line)
                (cond
                 ((looking-at "ChangeDirectory(\"\\(.*\\)\");")
                  (let ((newdir (match-string-no-properties 1)))
                    (magma--debug2-message (format "Magma scan: cd %s" newdir))
                    (setq magma-working-directory
                          (f-expand newdir magma-working-directory))
                    (magma--debug2-message (format "Magma scan: new workdir %s" magma-working-directory))))
                 ((looking-at "load \"\\(.*\\)\";")
                  (let* ((file (match-string-no-properties 1))
                         (filewithpath (f-expand file
                                                 magma-working-directory)))
                    (if (f-exists? filewithpath)
                      (save-excursion
                        (end-of-line)
                        (insert "\n")
                        (insert-file-contents filewithpath)
                        (magma--debug2-message
                         (format "Magma scan: loaded %s" filewithpath)))
                      (magma--debug2-message
                       (format "Magma scan: nonexistent file %s" filewithpath)))
                    ))
                 ((looking-at
                   "\\(function\\|procedure\\|intrinsics\\)[[:space:]]+\\(\\sw+\\)[[:space:]]*(")
                  (let ((newdef (match-string-no-properties 2)))
                    (magma--debug2-message (format "Magma scan: new definition found : %s" newdef))
                    (setq defs
                          (-union (list newdef) defs))
                  )))
                 
                (end-of-line) ;; So that forward-line really goes to the next line
                (setq moreLines (= 0 (forward-line 1)))
                )
              
              (cons magma-working-directory defs))))
       (setq magma-working-directory (car res))
       (cdr res)))

(provide 'magma-scan)

;;; magma-scan.el ends here
