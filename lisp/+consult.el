;;; +consult.el --- consult additions                -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun +consult-line ()
  "Call consult-line, using region as initial input, if active."
  (interactive)
  (if (use-region-p)
      (progn
        (deactivate-mark)
        (consult-line (buffer-substring-no-properties (region-beginning) (region-end))))
    (consult-line)))

;; Not currently used.
(defun +consult-ripgrep ()
  "Call consult-repgrep, using region as initial input, if active."
  (interactive)
  (if (use-region-p)
      (progn
        (deactivate-mark)
        (consult-ripgrep nil (buffer-substring-no-properties (region-beginning) (region-end))))
    (consult-ripgrep)))

(provide '+consult)
;;; +consult.el ends here
