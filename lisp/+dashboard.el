;;; +dashboard.el --- Additions for dashboard        -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(declare-function dashboard-insert-heading "dashboard")
(declare-function dashboard-insert-shortcut "dashboard")
(declare-function dashboard-insert-center "dashboard")

;; For the `dashboard-insert-section' macro.
(require 'dashboard)

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
  (let* ((cmd (format "apt 2>/dev/null list --upgradable | %s"
                      (expand-file-name "bin/aptglance.rb" user-emacs-directory)))
         (output (string-trim (shell-command-to-string cmd))))
    (if (not (string-empty-p output))
        (progn (dashboard-insert-heading "Pending APT Upgrades")
               (insert "\n")
               (dashboard-insert-center output)
               (insert "\n"))
      ;; Ugly hack to pretend section doesn't exist.
      (delete-char -1)
      (pop dashboard--section-starts))))

(defconst +dashboard-warning-regexp
  (rx bol (*? nonl)
      (group (or "Emergency" "Error" "Warning" "Debug")
             (? " (" (* (not ")")) ")")
             ": "))
  "Regexp matching the start of a warning in the *Warnings* buffer.")

(defun +dashboard-warnings ()
  "Return the first line of each warning in the *Warnings* buffer.
The position of the warning is in the `+warning-pos' text property."
  (when-let* ((buffer (get-buffer "*Warnings*")))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (let (warnings)
          (while (re-search-forward +dashboard-warning-regexp nil t)
            (push (propertize (buffer-substring-no-properties (match-beginning 1)
                                                              (line-end-position))
                              '+warning-pos (match-beginning 1))
                  warnings))
          (nreverse warnings))))))

(defun +dashboard-goto-warning (pos)
  "Show the warning at POS in the *Warnings* buffer."
  (pop-to-buffer "*Warnings*")
  (goto-char pos))

(defun +dashboard-insert-warnings (list-size)
  "Insert up to LIST-SIZE warnings from the *Warnings* buffer.
Inserts nothing if there's no warnings."
  (if-let* ((warnings (+dashboard-warnings)))
      (dashboard-insert-section
       "Warnings:"
       warnings
       list-size
       '+warnings
       "w"
       `(lambda (&rest _)
          (+dashboard-goto-warning ,(get-text-property 0 '+warning-pos el)))
       el)
    ;; Ugly hack to pretend section doesn't exist.
    (delete-char -1)
    (pop dashboard--section-starts)))

(provide '+dashboard)
;;; +dashboard.el ends here
