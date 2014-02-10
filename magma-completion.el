;; Completion in the magma buffer.

;; Base settings

(require 'magma-scan)

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

(defvar-local magma-completion-table nil)
  
(defun magma-completion-at-point ()
  (let* ((bounds (bounds-of-thing-at-point 'word))
         (start (car bounds))
         (end (cdr bounds)))
    (list start end magma-completion-table :exclusive 'no)))

(defun magma-init-completion ()
  "Function run at mode initialisation, activating the completion and defining
 its initial dictionary."
  (interactive)
  (setq magma-completion-table magma-completion-table-base)
  (make-local-variable 'completion-at-point-functions)
  (setq completion-at-point-functions (list 'magma-completion-at-point))
  )

(defun magma-interactive-add-to-completion-table (str)
  "Parse the string str, and extract new symbols to add to the completion table"
  (magma--debug-message "Scanning input for completion candidates...")
  (magma--debug-message (format "Input : %s" str))
  (let ((new-candidates
         (with-temp-buffer
           (insert str)
           (magma-mode)
           (magma-scan))))
    (magma--debug-message (format "Candidates found : %s" new-candidates))
    (setq magma-completion-table
          (-union new-candidates magma-completion-table))))

(defun magma-interactive-init-completion ()
  (magma-init-completion)
  (magma-interactive-rebuild-completion-table)
  (add-hook 'comint-input-filter-functions
            'magma-interactive-add-to-completion-table))


(defun magma-editor-init-completion ()
  (magma-init-completion)
  (magma-editor-rebuild-completion-table)
  )

(add-hook 'magma-comint-interactive-mode-hook 'magma-interactive-init-completion)

(add-hook 'magma-mode-hook 'magma-editor-init-completion)


(defun magma-editor-rebuild-completion-table ()
  (interactive)
  (magma--debug-message "Rebuilding the completion table...")
  (let ((new-candidates (magma-scan)))
    (setq magma-completion-table
        (-union new-candidates
                magma-completion-table)))
    nil)


(defun magma-interactive-rebuild-completion-table ()
  (interactive)
  nil)

(defun magma-completion-at-point ()
  (magma-editor-rebuild-completion-table)
  ;; Fixme: maybe rebuild only if called twice, or something...
  (completion-at-point))

(provide 'magma-completion)
