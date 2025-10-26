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

  (it "jumps to matching parenthesis, when point is after"
    (+test-with-temp-buffer
     " (|    ) " ()
     (+jump-to-matching)
     (+expect-buffer-equals " (    )| ")
     ;; And bact to the original.
     (+jump-to-matching)
     (+expect-buffer-equals " (|    ) "))))
