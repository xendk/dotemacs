;;; +buttercup.el --- Buttercup support                 -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'buttercup)

(defun +buttercup-minor-mode-maybe ()
  "Enable buttercup-minor-mode in current buffer if buttercup is required."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward "(require 'buttercup)" 100 t)
      (buttercup-minor-mode))))

(provide '+buttercup)
;;; +buttercup.el ends here
