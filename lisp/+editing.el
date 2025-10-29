;;; +editing.el --- One map to rule them all  -*- lexical-binding: t; -*-

;;; Commentary:

;; Misc editing functions I've cooked up over the years.
;;; Code:

(defun +open-line ()
  "Open new line, with proper indentation."
  (interactive)
  (beginning-of-line)
  (call-interactively 'open-line)
  (indent-for-tab-command))

(defun +mark-lines ()
  "Mark the current line, or expand the selection to another line.

Actually shrinks the region if the point is at the start of the region."
  (interactive)
  (let ((start (point)))
    (progn
      (if (not (region-active-p))
          (progn
            (beginning-of-line)
            (set-mark (point))
            (goto-char start)))
      (end-of-line)
      (forward-char))))

(defun +edit-init ()
  "Open init.el."
  (interactive)
  (find-file (locate-user-emacs-file "init.el")))

(defun +edit-tips ()
  "Open tips file."
  (interactive)
  (find-file (locate-user-emacs-file "tips")))

(defun +edit-todo ()
  "Open todo file."
  (interactive)
  (find-file (locate-user-emacs-file "todo")))


(provide '+editing)
;;; +editing.el ends here
