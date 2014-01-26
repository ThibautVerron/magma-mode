(ert-deftest magma-build-completion-table ()
    (with-temp-buffer
      (insert-file-contents-literally (f-join magma-path "testfiles/defuns.m"))
      (magma-mode)
      (magma-editor-rebuild-completion-table)
      (should (memq "test3" magma-completion-table))
      (should (memq "SetVerbose" magma-completion-table))))

(ert-deftest magma-build-completion-table-at-init ()
  (with-temp-buffer
    (insert-file-contents-literally (f-join magma-path "testfiles/defuns.m"))
    (magma-mode)
    (should (memq "test3" magma-completion-table))
    (should (memq "SetVerbose" magma-completion-table))))

(ert-deftest magma-build-completion-table-when-idle ()
  (with-temp-buffer
    (insert-file-contents-literally
     (f-join magma-path "testfiles/defuns.m"))
    (magma-mode)
    (insert-file-contents-literally
     (f-join magma-path "testfiles/extradefuns.m"))
    (sleep-for 35)
    (should (memq "test3" magma-completion-table))
    (should (memq "extratest1" magma-completion-table))
    (should (memq "SetVerbose" magma-completion-table))))


