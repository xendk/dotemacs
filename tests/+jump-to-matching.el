;;; -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'tests/+helpers)
(require '+jump-to-matching)

(describe "+jump-to-matching"
  (it "jumps to matching parenthesis"
    (+test-with-temp-buffer
     " |(    ) " ()
     (+jump-to-matching)
     (+expect-buffer-equals " (    |) ")
     ;; And bact to the original.
     (+jump-to-matching)
     (+expect-buffer-equals " |(    ) ")))

  (it "throws, when point is after"
    (+test-with-temp-buffer
     " (|    ) " ()
     (expect (+jump-to-matching) :to-throw)))

  (describe "with sp-show-pair-from-inside"
    (it "jumps to matching parenthesis, when point is after"
      (defvar sp-show-pair-from-inside)
      (let ((sp-show-pair-from-inside t))
        (+test-with-temp-buffer
         " (|    ) " ()
         (+jump-to-matching)
         (+expect-buffer-equals " (    )| ")
         ;; And bact to the original.
         (+jump-to-matching)
         (+expect-buffer-equals " (|    ) "))))))
