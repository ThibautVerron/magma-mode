(ert-deftest magma-build-completion-table ()
    (with-temp-buffer
      (insert-file-contents-literally (f-join magma-path "testfiles/defuns.m"))
      (magma-mode)
      (magma-editor-rebuild-completion-table)
      (should (-contains? magma-completion-table "test3"))
      (should (-contains? magma-completion-table "SetVerbose"))))

(ert-deftest magma-build-completion-table-at-init ()
  (with-temp-buffer
    (insert-file-contents-literally (f-join magma-path "testfiles/defuns.m"))
    (magma-mode)
    (should (-contains? magma-completion-table "test3"))
    (should (-contains? magma-completion-table "SetVerbose"))))

(ert-deftest magma-build-completion-table-on-demand ()
  (with-temp-buffer
    (insert-file-contents-literally
     (f-join magma-path "testfiles/defuns.m"))
    (magma-mode)
    (insert-file-contents-literally
     (f-join magma-path "testfiles/extradefuns.m"))
    (magma-editor-rebuild-completion-table)
    (should (-contains? magma-completion-table "test3"))
    (should (-contains? magma-completion-table "extratest1"))
    (should (-contains? magma-completion-table "SetVerbose"))))

(ert-deftest magma-build-completion-table-when-idle ()
  (with-temp-buffer
    (insert-file-contents-literally
     (f-join magma-path "testfiles/defuns.m"))
    (magma-mode)
    (insert-file-contents-literally
     (f-join magma-path "testfiles/extradefuns.m"))
    (ert-run-idle-timers)
    (should (-contains? magma-completion-table "test3"))
    (should (-contains? magma-completion-table "extratest1"))
    (should (-contains? magma-completion-table "SetVerbose"))))


