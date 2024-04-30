;;; +consult.el --- consult additions                -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

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
  "Variant of `consult-buffer' that groups by project."
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
  (if transform
      cand
    (let* ((default-directory (buffer-local-value 'default-directory (get-buffer (cdr (get-text-property 0 'multi-category cand)))))
           (project (project-current nil)))
      (if project
          (project-root project)
        "<no project>"))))

(provide '+consult)
;;; +consult.el ends here
