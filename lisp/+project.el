;;; +project.el --- project additions                -*- lexical-binding: t; -*-

;;; commentary:

;;; Code:

(defun +project-switch-to-shell ()
  "Switch to shell buffer in project. Use completion if multiple buffers."
  (interactive)
  (if (project-current)
      ;; Switch to project root in case we create a new shell buffer.
      (let ((default-directory (project-root (project-current t))))
        (+vterm-switch-to-shell (project-buffers (project-current))))
    (message "No project.")))

(defun +project-vterm ()
  "Start new vterm session in project root."
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (vterm)))

(provide '+project)
;;; +project.el ends here
