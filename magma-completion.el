(defvar magma-completion-table-file (f-join magma-path "data/magma_symbols.txt"))

(defconst magma-completion-table-base
  (with-temp-buffer
    (insert-file-contents magma-completion-table-file)
      (split-string (buffer-string) "\n" t)))

(defun magma-completion-at-point ()
  (let* ((bounds (bounds-of-thing-at-point 'word))
         (start (car bounds))
         (end (cdr bounds)))
    (list start end magma-completion-table-base)))

(add-hook 'magma-interactive-mode-hook
          (lambda () (add-to-list 'completion-at-point-functions 'magma-completion-at-point)))

