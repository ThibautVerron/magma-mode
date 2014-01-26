(add-to-list 'load-path (f-dirname (f-dirname (f-this-file))))

(require 'magma-mode)

(setq magma-interactive-program
      (let ((ext (if (eq system-type 'windows-nt) ".bat" ".sh")))
        (f-join magma-path "bin" (s-concat "dummymagma" ext))))
(setq magma-completion-table-file (f-join magma-path
                                          "data/dummymagma_symbols.txt"))



