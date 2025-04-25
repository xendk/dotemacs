;;; +consult.el --- consult additions                -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(declare-function consult--multi "consult")
(declare-function consult-line "consult")
(declare-function consult-ripgrep "consult")
(declare-function consult--buffer-action "consult")
(declare-function project-root (PROJECT) "project")
(defvar consult-buffer-sources)

(defun +consult-line ()
  "Call consult-line, using region as initial input, if active."
  (interactive)
  (if (use-region-p)
      (progn
        (deactivate-mark)
        (consult-line (buffer-substring-no-properties (region-beginning) (region-end))))
    (consult-line)))

;; Not currently used.
(defun +consult-ripgrep ()
  "Call consult-repgrep, using region as initial input, if active."
  (interactive)
  (if (use-region-p)
      (progn
        (deactivate-mark)
        (consult-ripgrep nil (buffer-substring-no-properties (region-beginning) (region-end))))
    (consult-ripgrep)))

(defun +consult-buffer-by-project (&optional sources)
  "Variant of `consult-buffer' that groups by project.

SOURCES is passed to consult--multi."
  (interactive)
  (let ((selected (consult--multi (or sources consult-buffer-sources)
                                  :require-match
                                  (confirm-nonexistent-file-or-buffer)
                                  :prompt "Switch to: "
                                  :history 'consult--buffer-history
                                  :sort nil
                                  :group #'+consult--multi-group-by-project
                                  )))
    ;; For non-matching candidates, fall back to buffer creation.
    (unless (plist-get (cdr selected) :match)
      (consult--buffer-action (car selected)))))

(defun +consult--multi-group-by-project (cand transform)
  "Group candidates by project.

Return project for CAND or TRANSFORM the candidate."
  (if transform
      cand
    ;; When using p <spc> to narrow buffers to current project,
    ;; there's no buffer for the candidates for other projects that
    ;; `consult-buffer' shows. So in this case we return the default
    ;; "Project Root" string and hope it fits.
    (let ((buffer (get-buffer (cdr (get-text-property 0 'multi-category cand)))))
      (if buffer
          (let* ((default-directory (buffer-local-value 'default-directory buffer))
                 (project (project-current nil)))
            (if project
                (project-root project)
              "<no project>"))
        "Project Root"))))

(provide '+consult)
;;; +consult.el ends here
