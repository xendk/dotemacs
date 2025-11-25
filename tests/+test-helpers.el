;;; -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'tests/+helpers)


(describe "+with-selected-region"
  (it "marks the given region"
    (+with-temp-buffer
     "one two three"
     (transient-mark-mode)
     (+with-selected-region
      "two"
      (expect (region-active-p) :to-be t)
      (+expect-buffer-equals "one |twoM three")))))
