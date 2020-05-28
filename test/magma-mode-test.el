;; -*- lexical-binding: t; -*-

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
  :expected-result :failed ;; Removed feature
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

(ert-deftest magma-scan-load ()
  (with-temp-buffer
    (let ((magma-working-directory (f-join magma-path "testfiles")))
      (insert-file-contents-literally
       (f-join magma-working-directory "load.m"))
      (magma-mode)
      (should (-contains? magma-completion-table "test1")))))

(ert-deftest magma-scan-cd ()
  (with-temp-buffer
    (let* ((magma-working-directory (f-join magma-path "testfiles"))
           (magma-base-working-directory (f-join magma-path "testfiles")))
      (insert-file-contents-literally
       (f-join magma-base-working-directory "changedirectory.m"))
      (magma-mode)
      (should (equal magma-working-directory
                     (f-join magma-base-working-directory "testdir")))
      (should (-contains? magma-completion-table "test2")))))
