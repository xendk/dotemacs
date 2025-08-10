;;; +with-simulated-input.el --- Assess support                 -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun +with-simulated-input-maybe-require ()
  "Require with-simulated-input if it is required by file."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward "(require 'with-simulated-input)" 100 t)
      (require 'with-simulated-input))))

(provide '+with-simulated-input)
;;; +with-simulated-input.el ends here
