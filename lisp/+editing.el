;;; +editing.el --- One map to rule them all  -*- lexical-binding: t; -*-

;;; Commentary:

;; Misc editing functions I've cooked up over the yeors.
;;; Code:

(defun +open-line ()
  "Open new line, with proper indentation."
  (interactive)
  (beginning-of-line)
  (call-interactively 'open-line)
  (indent-for-tab-command))

(provide '+editing)
;;; +editing.el ends here
