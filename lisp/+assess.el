;;; +buttercup.el --- Assess support                 -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun +assess-maybe-require ()
  "Enable buttercup-minor-mode in current buffer if buttercup is required."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward "(require 'assess)" 100 t)
      (require 'assess))))

(provide '+assess)
;;; +assess.el ends here
