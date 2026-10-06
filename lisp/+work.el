;;; +work.el --- support code for work.org           -*- lexical-binding: t; -*-

;;; Commentary:

;; Minor mode and helper functions for my work.org file.

;;; Code:

(defvar +work-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-c C-h") '+work-set-harvest-project)
    (define-key keymap (kbd "C-c h") '+work-harvest-clock-in)
    keymap)
  "Keymap for +work-mode.")

(define-minor-mode +work-mode
  "Minor mode for work.org."
  :keymap +work-mode-map)

(defun +work-set-harvest-project ()
  "Set Harvest project and task properties for the current org item."
  (interactive)
  (require 'reaper)
  (reaper-ensure-project-tasks)
  (let* ((prop-project-id (org-entry-get nil "HARVEST_PROJECT_ID"))
         (prop-task-id (org-entry-get nil "HARVEST_TASK_ID"))
         (default-project-id (when prop-project-id (string-to-number prop-project-id)))
         (default-task-id (when prop-task-id (string-to-number prop-task-id)))
         (project (reaper-with-buffer (reaper-read-project default-project-id)))
         (project-id (reaper-project-id project))
         (task-id (reaper-with-buffer (reaper-read-task project default-task-id))))
    (when (and project-id task-id)
      (org-entry-put nil "HARVEST_PROJECT_ID" (number-to-string project-id))
      (org-entry-put nil "HARVEST_TASK_ID" (number-to-string task-id)))))

(defun +work-harvest-clock-in ()
  "Clock in to Harvest based on current org item's properties and title.
If a timer for today matching the project, task, and title exists,
restart it. Otherwise, create a new timer."
  (interactive)
  (require 'reaper)
  (let ((prop-project-id (org-entry-get nil "HARVEST_PROJECT_ID"))
        (prop-task-id (org-entry-get nil "HARVEST_TASK_ID")))
    (unless (and prop-project-id prop-task-id)
      (user-error "No Harvest properties (HARVEST_PROJECT_ID, HARVEST_TASK_ID) found on current item"))
    (let* ((heading (org-get-heading t t t t))
           (clean-heading (if heading (string-trim heading) ""))
           (title (if (string-match "^\\([A-Za-z0-9]+-[0-9]+\\)" clean-heading)
                      (match-string 1 clean-heading)
                    clean-heading)))
      (when (string-empty-p title)
        (user-error "No title found for current item"))
      (let* ((project-id (string-to-number prop-project-id))
             (task-id (string-to-number prop-task-id))
             (existing-entry nil))
        (reaper-with-buffer
         (let ((today (format-time-string "%Y-%m-%d")))
           (unless (string= reaper-date today)
             (reaper--goto-date today)
             ;; If we just switched date, timeentries might be nil until async fetch completes.
             ;; We can just proceed, though existing-entry will be nil.
             ))
         (catch 'found
           (dolist (item reaper-timeentries)
             (let* ((entry (cdr item))
                    (e-proj-id (reaper-entry-project-id entry))
                    (e-task-id (reaper-entry-task-id entry))
                    (e-notes (reaper-entry-notes entry)))
               (when (and (eq e-proj-id project-id)
                          (eq e-task-id task-id)
                          (stringp e-notes)
                          (string-search title e-notes))
                 (setq existing-entry entry)
                 (throw 'found t)))))
         (if existing-entry
             (let ((entry-id (reaper-entry-id existing-entry)))
               (reaper-api "PATCH" (format "time_entries/%s/restart" entry-id) nil "Started timer")
               (reaper-refresh)
               (message "Restarted Harvest timer for: %s" title))
           (let ((harvest-payload (make-hash-table :test 'equal)))
             (puthash "project_id" project-id harvest-payload)
             (puthash "task_id" task-id harvest-payload)
             (puthash "spent_date" (format-time-string "%Y-%m-%d") harvest-payload)
             (puthash "notes" title harvest-payload)
             (reaper-api "POST" "time_entries" harvest-payload "Started timer")
             (reaper-refresh)
             (message "Created a new Harvest entry for: %s" title))))))))

(provide '+work)
;;; +work.el ends here
