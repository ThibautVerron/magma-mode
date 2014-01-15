(require 'f)

(defvar magma-mode-support-path
  (f-dirname load-file-name))

(defvar magma-mode-features-path
  (f-parent magma-mode-support-path))

(defvar magma-mode-root-path
  (f-parent magma-mode-features-path))

(add-to-list 'load-path magma-mode-root-path)

(require 'espuds)
(require 'ert)

(Setup
 ;; Before anything has run
 ;;(setq condition-error-function )
 (setq magma-interactive-use-comint t)
 (setq magma-use-electric-newline t)
 (require 'magma-mode)
 (setq-default indent-tabs-mode nil)
 (setq magma-interactive-program
       (f-join magma-mode-root-path "bin/dummymagma"))
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )

(Fail
 
 )
