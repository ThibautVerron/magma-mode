(require 'imenu)

(defvar magma-defun-regexp "^\\(function\\|procedure\\|intrinsics\\)[[:space:]]+\\(\\sw+\\)[[:space:]]*(")

(setq magma-imenu-generic-expression
      (list (list nil magma-defun-regexp 2)))

;; (defun magma-imenu-prev-index-position ()
;;   (search-backward-regexp magma-defun-regexp (point-min) t))

;; (defun magma-imenu-extract-index-name ()
;;   (save-excursion
;;     (and (looking-at magma-defun-regexp)
;;          (match-string-no-properties 2))))

(provide 'magma-imenu)
