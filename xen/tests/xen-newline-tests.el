;; -*- lexical-binding: t -*-

(require 'php-mode)

(require 'xen)

(describe "xen-comment-is-empty"
  (it "handles comment at end of file with no newline"
    (xen-test-with-temp-php-buffer
     "<?php\n// |"
     (expect (xen-comment-is-empty) :to-be t)
     ))
  (it "recognises empty comments"
    (xen-test-with-temp-php-buffer
     "<?php\n// |\n"
     (expect (xen-comment-is-empty) :to-be t)
     )
    (xen-test-with-temp-php-buffer
     "<?php\n//   |   \n"
     (expect (xen-comment-is-empty) :to-be t)))
  (it "recognises non-empty comments"
    (xen-test-with-temp-php-buffer
     "<?php\n// t|\n"
     (expect (xen-comment-is-empty) :to-be nil)
     )
    (xen-test-with-temp-php-buffer
     "<?php\n// |x\n"
     (expect (xen-comment-is-empty) :to-be nil)))
  (it "returns nil when not in comment"
    (xen-test-with-temp-php-buffer
     "<?php\nt|x\n"
     (expect (xen-comment-is-empty) :to-be nil)))
  (it "handles elisp comments"
    (xen-test-with-temp-elisp-buffer
     "; |"
     (expect (xen-comment-is-empty) :to-be t))
    (xen-test-with-temp-elisp-buffer
     ";; |"
     (expect (xen-comment-is-empty) :to-be t))
    (xen-test-with-temp-elisp-buffer
     ";;; |"
     (expect (xen-comment-is-empty) :to-be t))
    (xen-test-with-temp-elisp-buffer
     ";; |t"
     (expect (xen-comment-is-empty) :to-be nil))))

(describe "xen-empty-comment-start"
  (it "returns comment start for single empty comment"
    (xen-test-with-temp-php-buffer
     "<?php

// |"
     (expect (xen-empty-comment-start) :to-be 8)))
  (it "returns comment start for single empty comment without whitespace"
    (xen-test-with-temp-php-buffer
     "<?php

//|"
     (expect (xen-empty-comment-start) :to-be 8)))
  (it "returns nil non-empty comment"
    (xen-test-with-temp-php-buffer
     "<?php

// t|"
     (expect (xen-empty-comment-start) :to-be nil)))
  (it "returns nil on first empty comment"
    (xen-test-with-temp-php-buffer
     "<?php

// t
// |"
     (expect (xen-empty-comment-start) :to-be nil)))
  (it "returns start of first on second empty comment"
    (xen-test-with-temp-php-buffer
     "<?php

// t
//
// |"
     (expect (xen-empty-comment-start) :to-be 13)))
  (it "shouldn't fail on first line of buffer"
    (xen-test-with-temp-php-buffer
     "// |"
     (expect (xen-empty-comment-start) :to-be nil)))

  (describe "in emacs-lisp-mode"
    (it "should handle multiple semis"
      (xen-test-with-temp-elisp-buffer
       "()\n;;|"
       (expect (xen-empty-comment-start) :to-be 4)))))

(describe "xen-newline"
  (describe "for php-mode"
    (it "should handle multiline comments"
      (xen-test-with-temp-php-buffer
       "<php
/**
 * |
 */"
       (xen-newline)
       (xen-expect-buffer-equals "<php
/**
 *
 * |
 */")))
    (it "should handle start of multiline comments"
      (xen-test-with-temp-php-buffer
       "<php\n/**|\n *\n */\n"
       (xen-newline)
       ;; Sadly there's not a space before point, but that's an issue
       ;; with `default-indent-new-line'.
       (xen-expect-buffer-equals "<php\n/**\n *|\n *\n */\n"))))
  (describe "for emacs-lisp-mode"
    (it "Handles insertion properly"
      (xen-test-with-temp-elisp-buffer
       ";; |\n"
       (xen-newline)
       (xen-expect-buffer-equals ";; \n;; |\n")
       ))
    (it "Handles deletion properly"
      (xen-test-with-temp-elisp-buffer
       ";; \n;; |\n"
       (xen-newline)
       (xen-expect-buffer-equals "|\n")
       ))))
