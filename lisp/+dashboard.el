;;; +dashboard.el --- Additions for dashboard        -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(declare-function dashboard-insert-heading "dashboard")
(declare-function dashboard-insert-shortcut "dashboard")
(declare-function dashboard-insert-center "dashboard")

(defvar dashboard--section-starts)

(defun +dashboard-tip (_list-size)
  "Insert a tip into the dashboard.

LIST-SIZE is ignored."
  (dashboard-insert-heading "Tip of the day" "t")
  (insert "\n")
  (let ((tips (with-temp-buffer
                (insert-file-contents (locate-user-emacs-file "tips"))
                (split-string (buffer-string) "\f" t))))
    (dashboard-insert-center (elt tips (random (length tips)))))
  (dashboard-insert-shortcut 'tip "t" "Tip of the day"))

(defun +dashboard-insert-apt-upgrades (_list-size)
  "Insert a list of pending APT upgrades using aptglance.

_LIST-SIZE is ignored."
  (let* ((cmd "apt 2>/dev/null list --upgradable | /home/xen/dev/tools/aptglance/aptglance.rb")
         (output (string-trim (shell-command-to-string cmd))))
    (if (not (string-empty-p output))
        (progn (dashboard-insert-heading "Pending APT Upgrades")
               (insert "\n")
               (dashboard-insert-center output)
               (insert "\n"))
      ;; Ugly hack to pretend section doesn't exist.
      (delete-char -1)
      (pop dashboard--section-starts))))

(provide '+dashboard)
;;; +dashboard.el ends here
