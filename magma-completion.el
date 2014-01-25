;; Completion in the magma buffer.

;; Base settings

(defvar magma-completion-table-file (f-join magma-path "data/magma_symbols.txt"))
(defun magma-build-initial-table (file)
  (interactive)
  (with-temp-buffer
    (condition-case nil
        (insert-file-contents file)
      (error (message "The index file does not exist, so I cannot enable completion. Please see the comments to build it.")))
    (split-string (buffer-string) "\n" t)))

(defvar magma-completion-table-base
  (magma-build-initial-table magma-completion-table-file))
  
  
(defun magma-completion-at-point ()
  (let* ((bounds (bounds-of-thing-at-point 'word))
         (start (car bounds))
         (end (cdr bounds)))
    (list start end magma-completion-table)))

(defun magma-init-completion ()
  "Function run at mode initialisation, activating the completion and defining
 its initial dictionary."
  (interactive)
  (make-local-variable 'magma-completion-table)
  (setq magma-completion-table magma-completion-table-base)
  (make-local-variable 'completion-at-point-functions)
  (add-to-list 'completion-at-point-functions 'magma-completion-at-point)
  )

(defun magma-interactive-init-completion ()
  (magma-init-completion)
  (magma-interactive-rebuild-completion-table)
  )

(defcustom magma-completion-auto-update 30
  "Should we rescan the buffer for new candidates to completion?
  If nil, never rescan automatically, if a number n, rescan after
  n seconds of inactivity."
  )

(defun magma-editor-init-completion ()
  (magma-init-completion)
  (magma-editor-rebuild-completion-table)
  (when magma-completion-auto-update
    (run-with-idle-timer magma-completion-auto-update t 'magma-editor-rebuild-completion-table))
  )


(add-hook 'magma-comint-interactive-mode-hook 'magma-interactive-init-completion)
(add-hook 'magma-mode-hook 'magma-editor-init-completion)


(defun magma-editor-rebuild-completion-table ()
  (interactive)
  (setq magma-completion-table
        (append magma-completion-table
                (mapcar 'car (cdr (imenu--make-index-alist t))))))

(defun magma-interactive-rebuild-completion-table ()
  (interactive)
  nil)


(provide 'magma-completion)
