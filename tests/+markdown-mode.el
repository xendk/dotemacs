;;; -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'tests/+helpers)
(require '+markdown-mode)

(describe "+markdown-paste-link"
  (it "makes a markdown link of the selected reagion"
    (+with-temp-buffer
     "some inline text" (transient-mark-mode)
     (+with-selected-region "inline"
       (kill-new "a link")
       (+markdown-paste-link)
       (+expect-buffer-equals "some [inline](a link) text"))))

  (it "does nothing with no active region"
    (+with-temp-buffer
     "some inline text" (transient-mark-mode)
     (kill-new "a link")
     (+markdown-paste-link)
     (+expect-buffer-equals "some inline text"))))
