;; We want functions to scan a magma buffer for names (completion
;; candidates) and other information:
;; - working directory (following "ChangeDirectory" commands)
;; - names extracted from external files (loaded with "load")

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
