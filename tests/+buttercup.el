;;; -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'assess)

(require '+buttercup)

(describe "+buttercup"
  (describe "+buttercup-minor-mode-maybe"
    (it "does nothing if no (require 'buttercup)"
      (assess-as-temp-buffer ";;; Just a lispy file"
        (+buttercup-minor-mode-maybe)
        (expect buttercup-minor-mode :not :to-be-truthy)))

    (it "enables buttercup-minor-mode if (require 'buttercup) found"
      (assess-as-temp-buffer ";;; Just a lispy file
(require 'buttercup)"
        (+buttercup-minor-mode-maybe)
        (expect buttercup-minor-mode :to-be-truthy)))))
