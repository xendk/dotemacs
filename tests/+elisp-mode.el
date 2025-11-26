;;; -*- lexical-binding: t; -*-

(require 'buttercup)
(require '+elisp-mode)

(defmacro user-file (file)
  (concat (expand-file-name user-emacs-directory) file))

(describe "+elisp-mode-test-switch"
  (it "should do nothing in buffers without file"
    (spy-on 'find-file)
    (let ((buffer-file-name nil))
      (+elisp-mode-test-switch))
    (expect 'find-file :not :to-have-been-called))

  (it "should do nothing on files outside lisp/tests"
    (spy-on 'find-file)
    (let ((buffer-file-name (user-file "random-file.el")))
      (+elisp-mode-test-switch))
    (expect 'find-file :not :to-have-been-called))

  (it "switches from implementation to test"
    (spy-on 'find-file)
    (let ((buffer-file-name (user-file "lisp/+elisp-mode.el")))
      (+elisp-mode-test-switch))
    (expect 'find-file :to-have-been-called-with (user-file "tests/+elisp-mode.el")))

  (it "switches from test to implementation"
    (spy-on 'find-file)
    (let ((buffer-file-name (user-file "tests/+elisp-mode.el")))
      (+elisp-mode-test-switch))
    (expect 'find-file :to-have-been-called-with (user-file "lisp/+elisp-mode.el"))))
