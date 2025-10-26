;;; +highlight-symbol.el --- highlight-symbol additions  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(declare-function highlight-symbol-mode "highlight-symbol")
(defvar highlight-symbol-mode)

(defun +highlight-symbol-mode-deactivate ()
  "Temporarily disable highlight-symbol-mode."
  (when (bound-and-true-p highlight-symbol-mode)
    (setq-local highlight-symbol-mode-suspend t)
    (highlight-symbol-mode 0)))

(defun +highlight-symbol-mode-reactivate ()
  "Reactive highlight-symbol-mode after we deactivated it.

Undo the work of `+highlight-symbol-mode-deactivate'."
  (when (bound-and-true-p highlight-symbol-mode-suspend)
    (kill-local-variable highlight-symbol-mode-suspend)
    (highlight-symbol-mode 1)))

(provide '+highlight-symbol)
;;; +highlight-symbol.el ends here
