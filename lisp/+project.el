;;; +project.el --- project additions                -*- lexical-binding: t; -*-

;;; commentary:

;;; Code:

(declare-function project-root "project")
(declare-function project-buffers "project")
(declare-function project-name "project")
(declare-function +vterm-switch-to-shell "+vterm")
(declare-function vterm "vterm")

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

(defun +project-kill-buffers-of-all-projects ()
  "Kill buffers of all open projects."
  (interactive)
  (let ((projects '())
        (names '()))
    (dolist (buffer (buffer-list))
      (let* ((default-directory (buffer-local-value 'default-directory buffer))
             (project (project-current nil)))
        (when project
          (setq projects (cons project projects))
          (project-root project)))
      )
    (delete-dups projects)
    (dolist (project projects names)
      (setq names (cons (project-name project) names)))
    (when (and projects
               (yes-or-no-p
                (format "Kill all buffers in %s? "
                        (cond
                         ((= (length names) 1) (format "project %s" (car names)))
                         (t (let ((first-parts (butlast names 1))
                                  (last-part (car (last names))))
                              (format "projects %s and %s" (mapconcat 'identity first-parts ", ") last-part)))))))
      (dolist (project projects)
        (let ((default-directory (project-root project)))
          (message "killing %s" (project-name project))
          (project-kill-buffers t))))))

(provide '+project)
;;; +project.el ends here
