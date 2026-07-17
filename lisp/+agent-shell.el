;;; +agent-shell.el --- agent-shell additions             -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun +agent-shell-mode-setup ()
  "Mode hook for `agent-shell-mode'."
  ;; Don't preselect first match, it seems odd here.
  (setq-local corfu-preselect 'prompt)
  ;; Completion preview mode shifts the corfu popup.
  (completion-preview-mode -1))

(provide '+agent-shell)
;;; +agent-shell.el ends here
