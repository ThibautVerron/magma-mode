;; Completion in the magma buffer.
;;
;; No customization is available for now. The completion engine
;; depends on an index file it expects to find in
;; <path_to_magma-mode>/data/magma_symbols.txt, and this file should
;; contain completion candidates, one per line. It is not clear
;; whether it is legally possible to distribute such a file, but in
;; the future, we will at least provide a script to build it from the
;; magma documentation files.


(defvar magma-completion-table-file (f-join magma-path "data/magma_symbols.txt"))

(defun magma-scan-index (file)
  (interactive)
  (with-temp-buffer
    (condition-case nil
        (insert-file-contents file)
      (error (message "The index file does not exist, so I cannot enable completion. Please see the comments to build it.")))
    (split-string (buffer-string) "\n" t)))


(defvar magma-completion-table-base
  (magma-scan-index magma-completion-table-file))
  
  
(defun magma-completion-at-point ()
  (let* ((bounds (bounds-of-thing-at-point 'word))
         (start (car bounds))
         (end (cdr bounds)))
    (list start end magma-completion-table-base)))

(add-hook 'magma-comint-interactive-mode-hook
          (lambda () (add-to-list 'completion-at-point-functions 'magma-completion-at-point)))

(add-hook 'magma-mode-hook
          (lambda () (add-to-list 'completion-at-point-functions 'magma-completion-at-point)))


(provide 'magma-completion)
