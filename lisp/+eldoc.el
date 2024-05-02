;;; +eldoc.el --- eldoc additions                    -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun +eldoc-window-select (orig &rest args)
  "Select eldoc window."
  (select-window (apply orig args)))

(advice-add 'eldoc-doc-buffer :around #'+eldoc-window-select)

(provide '+eldoc)
;;; +eldoc.el ends here
