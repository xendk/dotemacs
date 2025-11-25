;;; -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'tests/+helpers)
(require '+commenting-newline)

(describe "+commenting-newline-comment-line-is-empty"
  (it "handles comment at end of file with no newline"
    (+with-temp-php-buffer
     "<?php\n// |"
     (expect (+commenting-newline-comment-line-is-empty) :to-be t)
     ))
  (it "recognises empty comments"
    (+with-temp-php-buffer
     "<?php\n// |\n"
     (expect (+commenting-newline-comment-line-is-empty) :to-be t)
     )
    (+with-temp-php-buffer
     "<?php\n//   |   \n"
     (expect (+commenting-newline-comment-line-is-empty) :to-be t)))
  (it "recognises non-empty comments"
    (+with-temp-php-buffer
     "<?php\n// t|\n"
     (expect (+commenting-newline-comment-line-is-empty) :to-be nil)
     )
    (+with-temp-php-buffer
     "<?php\n// |x\n"
     (expect (+commenting-newline-comment-line-is-empty) :to-be nil)))
  (it "returns nil when not in comment"
    (+with-temp-php-buffer
     "<?php\nt|x\n"
     (expect (+commenting-newline-comment-line-is-empty) :to-be nil)))
  (it "handles elisp comments"
    (+with-temp-elisp-buffer
     "; |"
     (expect (+commenting-newline-comment-line-is-empty) :to-be t))
    (+with-temp-elisp-buffer
     ";; |"
     (expect (+commenting-newline-comment-line-is-empty) :to-be t))
    (+with-temp-elisp-buffer
     ";;; |"
     (expect (+commenting-newline-comment-line-is-empty) :to-be t))
    (+with-temp-elisp-buffer
     ";; |t"
     (expect (+commenting-newline-comment-line-is-empty) :to-be nil))))

(describe "+commenting-newline-empty-comment-start"
  (it "returns comment start for single empty comment"
    (+with-temp-php-buffer
     "<?php

// |"
     (expect (+commenting-newline-empty-comment-start) :to-be 8)))
  (it "returns comment start for single empty comment without whitespace"
    (+with-temp-php-buffer
     "<?php

//|"
     (expect (+commenting-newline-empty-comment-start) :to-be 8)))
  (it "returns nil on non-empty comment"
    (+with-temp-php-buffer
     "<?php

// t|"
     (expect (+commenting-newline-empty-comment-start) :to-be nil)))
  (it "returns nil on first empty comment"
    (+with-temp-php-buffer
     "<?php

// t
// |"
     (expect (+commenting-newline-empty-comment-start) :to-be nil)))
  (it "returns start of first on second empty comment"
    (+with-temp-php-buffer
     "<?php

// t
//
// |"
     (expect (+commenting-newline-empty-comment-start) :to-be 13)))
  (it "shouldn't fail on comment on first line of buffer"
    (+with-temp-php-buffer
     "// |"
     (expect (+commenting-newline-empty-comment-start) :to-be 1)))

  (it "shouldn't fail on first line of buffer"
    (+with-temp-php-buffer
     "bob |"
     (expect (+commenting-newline-empty-comment-start) :to-be nil)))

  (describe "in emacs-lisp-mode"
    (it "should handle multiple semis"
      (+with-temp-elisp-buffer
       "()\n;;|"
       (expect (+commenting-newline-empty-comment-start) :to-be 4)))))

(describe "+commenting-newline"
  (describe "for php-mode"
    (it "should handle multiline comments"
      (+with-temp-php-buffer
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
      (+with-temp-php-buffer
       "<php\n/**|\n *\n */\n"
       (+commenting-newline)
       ;; Sadly there's not a space before point, but that's an issue
       ;; with `default-indent-new-line'.
       (+expect-buffer-equals "<php\n/**\n *|\n *\n */\n"))))

  (describe "for emacs-lisp-mode"
    (it "Handles insertion properly"
      (+with-temp-elisp-buffer
       ";; x|\n"
       (+commenting-newline)
       (+expect-buffer-equals ";; x\n;; |\n")))

    (it "Handles deletion properly"
      (+with-temp-elisp-buffer
       ";; \n;; |\n"
       (+commenting-newline)
       (+expect-buffer-equals "|\n")))))
