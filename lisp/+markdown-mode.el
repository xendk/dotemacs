;;; +markdown-mode.el --- markdown-mode additions    -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(defun +markdown-paste-link ()
  "Make region a Markdown link using the URL in the kill ring."
  (interactive)
  (when (region-active-p)
    (with-restriction (region-beginning) (region-end)
      (goto-char (point-min))
      (insert "[")
      (goto-char (point-max))
      (insert "](" (current-kill 0) ")"))))

(provide '+markdown-mode)
;;; +markdown-mode.el ends here
