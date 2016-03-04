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

(require 'magma-vars)
(declare-function magma-mode "magma-mode.el")

(defvar-local magma-working-directory magma-default-directory)

(defun magma-scan-completion-file (file)
  (interactive)
  (with-temp-buffer
    (condition-case nil
        (insert-file-contents file)
      (error (message "The index file does not exist, so I cannot enable completion. Please see the comments to build it.")))
    (split-string (buffer-string) "\n" t)))

(defconst magma-scan-defun-regexp "\\(function\\|procedure\\|intrinsics\\)[[:space:]]+\\(\\sw+\\)[[:space:]]*(")

(defun magma-scan-make-filename (file)
  "Make the name of the file holding the completion candidates
  for the file FILE. If FILE is nil, make a name based on the
  current buffer's name."
  (if file
      (let* ((fullfile (f-long file))
             (path (f-dirname fullfile))
             (base (f-filename fullfile)))
        (f-join path (concat ".scan-" base ".el")))
    ;; If the buffer isn't associated to any file...
    (let ((buf (buffer-name)))
      (f-join "/tmp"
              (concat ".scan-"
                      (substring-no-properties buf 1 -1) ;; Dirty...
                      ".el")))))

(defun magma-scan-changedirectory-el (dir)
  "Elisp code to insert to perform a cd to DIR from the current directory held in magma-working-directory"
  (concat "(setq magma-working-directory (f-expand \"" dir "\" magma-working-directory))\n"))

(defun magma-scan-load-el (file)
  "Elisp code to insert to load the definitions from another file"
  (concat "(magma-load-or-rescan (f-expand \"" file "\" magma-working-directory))\n"))

(defun magma-scan-write-to-file (text file &optional overwrite)
  (let ((append (not overwrite)))
    (write-region text nil file append 'nomessage)))

(defun magma-scan-file (file outfile)
  "Scan the file file for definitions, and write the result into file OUTFILE."
  (magma-scan-write-to-file ";;; This file was generated automatically.\n\n" outfile t)
  (let* ((buf (current-buffer))
         (alldefs
          (let ((moreLines nil)
                (defs nil))
            (with-temp-buffer
              (let ((magma-mode-hook nil))
                (magma-mode))
              (insert "\n")
              (if file
                  (insert-file-contents file)
                (insert-buffer-substring-no-properties buf))
              (goto-char (point-min))
              
              ;; Get rid of the comments
              (magma--comment-kill-no-kill-ring (count-lines (point-min) (point-max)))
              (goto-char (point-min))
              
              ;; And scan
              (setq moreLines t)
              (setq defs nil)
              
              (while moreLines
                (beginning-of-line)
                (cond
                 ((looking-at "ChangeDirectory(\"\\(.*\\)\");")
                  (magma-scan-write-to-file
                   (magma-scan-changedirectory-el
                    (match-string-no-properties 1))
                   outfile))
                 ((looking-at "load \"\\(.*\\)\";")
                  (let* ((file (match-string-no-properties 1)))
                    (magma-scan-write-to-file (magma-scan-load-el file)
                                              outfile)))
                 ((looking-at magma-scan-defun-regexp)
                  (setq defs
                        (-union (list (match-string-no-properties 2))
                                defs))
                  )
                 )
                (end-of-line) ;; So that forward-line really goes to the next line
                (setq moreLines (= 0 (forward-line 1))))
              defs))))
    (let ((defsline
            (concat "(setq magma-completion-table "
                    "(-union magma-completion-table '("
                    (-reduce-r-from
                     (apply-partially 'format "\"%s\" %s") "" alldefs)
                    ")))\n")))
      (magma-scan-write-to-file defsline outfile ))))

(defun magma-load-or-rescan (file &optional forcerescan)
  "Load the completion file associated to file, rebuilding it if needed.

If FILE is nil, always rebuild the table."
  (if (or (not file) (f-exists? file))
      (let ((loadfile (magma-scan-make-filename file)))
        (when (or forcerescan
                  (not file)
                  (file-newer-than-file-p file loadfile))
          (magma-scan-file file loadfile))
        (load loadfile nil t t))
    (magma--debug-message
     (format "Skipping nonexistent file %s" file))))

(defun magma-scan (&optional forcerescan)
  "Scan the current buffer for completions (unless it isn't needed)"
  (interactive "P")
  (magma-load-or-rescan (buffer-file-name) forcerescan)
  )

(defun magma-visit-scan ()
  (interactive)
  (let ((file (buffer-file-name)))
    (find-file-read-only-other-frame (magma-scan-make-filename file))))

(provide 'magma-scan)

;;; magma-scan.el ends here
