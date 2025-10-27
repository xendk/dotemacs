;;; -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'tests/+helpers)
(require '+commenting-newline)

(describe "+commenting-newline-comment-line-is-empty"
  (it "handles comment at end of file with no newline"
    (+test-with-temp-php-buffer
     "<?php\n// |"
     (expect (+commenting-newline-comment-line-is-empty) :to-be t)
     ))
  (it "recognises empty comments"
    (+test-with-temp-php-buffer
     "<?php\n// |\n"
     (expect (+commenting-newline-comment-line-is-empty) :to-be t)
     )
    (+test-with-temp-php-buffer
     "<?php\n//   |   \n"
     (expect (+commenting-newline-comment-line-is-empty) :to-be t)))
  (it "recognises non-empty comments"
    (+test-with-temp-php-buffer
     "<?php\n// t|\n"
     (expect (+commenting-newline-comment-line-is-empty) :to-be nil)
     )
    (+test-with-temp-php-buffer
     "<?php\n// |x\n"
     (expect (+commenting-newline-comment-line-is-empty) :to-be nil)))
  (it "returns nil when not in comment"
    (+test-with-temp-php-buffer
     "<?php\nt|x\n"
     (expect (+commenting-newline-comment-line-is-empty) :to-be nil)))
  (it "handles elisp comments"
    (+test-with-temp-elisp-buffer
     "; |"
     (expect (+commenting-newline-comment-line-is-empty) :to-be t))
    (+test-with-temp-elisp-buffer
     ";; |"
     (expect (+commenting-newline-comment-line-is-empty) :to-be t))
    (+test-with-temp-elisp-buffer
     ";;; |"
     (expect (+commenting-newline-comment-line-is-empty) :to-be t))
    (+test-with-temp-elisp-buffer
     ";; |t"
     (expect (+commenting-newline-comment-line-is-empty) :to-be nil))))

(describe "+commenting-newline-empty-comment-start"
  (it "returns comment start for single empty comment"
    (+test-with-temp-php-buffer
     "<?php

// |"
     (expect (+commenting-newline-empty-comment-start) :to-be 8)))
  (it "returns comment start for single empty comment without whitespace"
    (+test-with-temp-php-buffer
     "<?php

//|"
     (expect (+commenting-newline-empty-comment-start) :to-be 8)))
  (it "returns nil on non-empty comment"
    (+test-with-temp-php-buffer
     "<?php

// t|"
     (expect (+commenting-newline-empty-comment-start) :to-be nil)))
  (it "returns nil on first empty comment"
    (+test-with-temp-php-buffer
     "<?php

// t
// |"
     (expect (+commenting-newline-empty-comment-start) :to-be nil)))
  (it "returns start of first on second empty comment"
    (+test-with-temp-php-buffer
     "<?php

// t
//
// |"
     (expect (+commenting-newline-empty-comment-start) :to-be 13)))
  (it "shouldn't fail on comment on first line of buffer"
    (+test-with-temp-php-buffer
     "// |"
     (expect (+commenting-newline-empty-comment-start) :to-be 1)))

  (it "shouldn't fail on first line of buffer"
    (+test-with-temp-php-buffer
     "bob |"
     (expect (+commenting-newline-empty-comment-start) :to-be nil)))

  (describe "in emacs-lisp-mode"
    (it "should handle multiple semis"
      (+test-with-temp-elisp-buffer
       "()\n;;|"
       (expect (+commenting-newline-empty-comment-start) :to-be 4)))))

(describe "+commenting-newline"
  (describe "for php-mode"
    (it "should handle multiline comments"
      (+test-with-temp-php-buffer
       "<php
/**
 * |
 */"
       (+commenting-newline)
       (+expect-buffer-equals "<php
/**
 *
 * |
 */")))

    (it "should handle start of multiline comments"
      (+test-with-temp-php-buffer
       "<php\n/**|\n *\n */\n"
       (+commenting-newline)
       ;; Sadly there's not a space before point, but that's an issue
       ;; with `default-indent-new-line'.
       (+expect-buffer-equals "<php\n/**\n *|\n *\n */\n"))))

  (describe "for emacs-lisp-mode"
    (it "Handles insertion properly"
      (+test-with-temp-elisp-buffer
       ";; x|\n"
       (+commenting-newline)
       (+expect-buffer-equals ";; x\n;; |\n")))

    (it "Handles deletion properly"
      (+test-with-temp-elisp-buffer
       ";; \n;; |\n"
       (+commenting-newline)
       (+expect-buffer-equals "|\n")))))
