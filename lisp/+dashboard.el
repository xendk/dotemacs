;;; +dashboard.el --- Additions for dashboard        -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(declare-function dashboard-insert-heading "dashboard")
(declare-function dashboard-insert-shortcut "dashboard")

(defun +dashboard-tip (_list-size)
  "Insert a tip into the dashboard.

LIST-SIZE is ignored."
  (dashboard-insert-heading "Tip of the day" "t")
  (insert "\n")
  (let ((tips (with-temp-buffer
                (insert-file-contents (locate-user-emacs-file "tips"))
                (split-string (buffer-string) "\f" t))))
    (insert (elt tips (random (length tips)))))
  (dashboard-insert-shortcut 'tip "t" "Tip of the day"))

(provide '+dashboard)
;;; +dashboard.el ends here
