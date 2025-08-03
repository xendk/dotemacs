;;; -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'assess)

(require '+assess)

(describe "+assess"
  (describe "+assess-maybe-require"
    (it "does nothing if no (require 'assess)"
      (spy-on 'require)
      (assess-as-temp-buffer ";;; Just a lispy file"
        (+assess-maybe-require)
        (expect 'require :not :to-have-been-called)))

    (it "requires 'assess if (require 'assess) found"
      (spy-on 'require)
      (assess-as-temp-buffer ";;; Just a lispy file
(require 'assess)"
        (+assess-maybe-require)
        (expect 'require :to-have-been-called-with 'assess)))))
