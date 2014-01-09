;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(When "^I indent line \"\\([[:digit:]]+\\)\""
      (lambda (line)
        (When (format "I go to line \"%s\"" line))
        (magma-indent-line)
        )
      )

(When "^I indent the buffer"
      (lambda ()
        (mark-whole-buffer)
        (indent-region (point) (mark))
        ))

(When "^I cut the line \\(before \".+\"\\|after \".+\"\\|between \".+\" and \".+\"\\)"
      (lambda (position)
        (When (format "I place the cursor %s" position))
        (magma-newline-and-indent)))

;; Redefinitions of some steps from espuds

(Then "^I should see\\(?: \"\\(.+\\)\"\\|:\\)$"
  "Asserts that the current buffer includes some text."
  (lambda (expected)
    (let ((actual (buffer-string))
          (message "Expected \n\"\"\"\n%s\n\"\"\"\nto be part of \n\"\"\"\n%s\n\"\"\"\nbut was not."))
      (cl-assert (s-contains? expected actual) nil message expected actual))))

(Then "^I should not see\\(?: \"\\(.+\\)\"\\|:\\)$"
  "Asserts that the current buffer does not include some text."
  (lambda (expected)
    (let ((actual (buffer-string))
          (message "Expected \n\"\"\"\n%s\n\"\"\"\nto not be part of \n\"\"\"\n%s\n\"\"\"\nbut was."))
      (cl-assert (not (s-contains? expected actual)) nil message expected actual))))
