;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(Then "^the buffer should contain:$"
  "Asserts that the current matches some text."
  (lambda (expected)
    (let ((actual (buffer-string))
          (message "Expected '%s' to be equal to '%s', but was not."))
      (cl-assert (s-equals? expected actual) nil message expected actual))))