;;; +assess.el --- Assess support                 -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun +assess-maybe-require ()
  "Require assess if it is required by file."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward "(require 'assess)" 100 t)
      (require 'assess))))

(provide '+assess)
;;; +assess.el ends here
