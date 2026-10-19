;;; +review.el --- collect review notes per project  -*- lexical-binding: t; -*-

;;; Commentary:

;; Collect `file:line:column: note' lines in a per-project buffer, for
;; handing over to an LLM to execute.

;;; Code:

(require 'project)

(defvar +review-window-height 0.25
  "Height of the review window, as used by the `window-height' display action.")

(defvar +review-mode-map
  (let ((keymap (make-sparse-keymap)))
    (keymap-set keymap "C-c n" '+review-insert)
    keymap)
  "Keymap for `+review-mode'.")

(define-minor-mode +review-mode
  "Global minor mode for collecting review notes."
  :global t
  :keymap +review-mode-map)

(define-derived-mode +review-comments-mode text-mode "Review"
  "Major mode for review notes buffers.")

(defvar-local +review--project-root nil
  "Root of the project the review buffer belongs to.")
(put '+review--project-root 'permanent-local t)

(defun +review--buffer (project)
  "Return the review buffer of PROJECT, creating it if needed."
  (let ((root (project-root project)))
    (or (seq-find (lambda (buffer)
                    (with-current-buffer buffer
                      (and (derived-mode-p '+review-comments-mode)
                           (equal +review--project-root root))))
                  (buffer-list))
        (with-current-buffer (generate-new-buffer
                              (format "*%s review*" (project-name project)))
          (+review-comments-mode)
          (setq +review--project-root root
                default-directory root)
          (current-buffer)))))

(defun +review--display (buffer)
  "Display review BUFFER in a window on the selected frame and return it.

Reuses a window already showing BUFFER or another review buffer,
otherwise creates a new window at the bottom of the frame which
survives `delete-other-windows'."
  (let ((windows (window-list nil 'no-minibuf)))
    (or (get-buffer-window buffer)
        (when-let* ((window (seq-find
                             (lambda (window)
                               (with-current-buffer (window-buffer window)
                                 (derived-mode-p '+review-comments-mode)))
                             windows)))
          (set-window-buffer window buffer)
          window)
        (let ((window (display-buffer-at-bottom
                       buffer
                       `((window-height . ,+review-window-height)))))
          (unless window
            (user-error "Could not create review window"))
          (set-window-parameter window 'no-delete-other-windows t)
          window))))

(defun +review-insert ()
  "Add a review note for the current position.

Inserts a `file:line:column: ' line at the end of the project's
review buffer and selects its window, ready for typing the note."
  (interactive)
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file"))
  (let ((project (or (project-current)
                     (user-error "Not in a project"))))
    (let ((location (format "%s:%d:%d: "
                            (file-relative-name buffer-file-name
                                                (project-root project))
                            (line-number-at-pos)
                            (1+ (current-column))))
          (buffer (+review--buffer project)))
      (select-window (+review--display buffer))
      (goto-char (point-max))
      (unless (bolp)
        (insert "\n"))
      (insert location))))

(provide '+review)
;;; +review.el ends here
