;; We want functions to scan a magma buffer for names (completion
;; candidates) and other information:
;; - working directory (following "ChangeDirectory" commands)
;; - names extracted from external files (loaded with "load")


(defun magma-scan-get-names ()
  "Returns a list of the functions defined in the current buffer"
  (interactive)
  (let ((imenu--index-alist nil))
    (mapcar 'car (cdr (imenu--make-index-alist t)))))




(provide 'magma-scan)
