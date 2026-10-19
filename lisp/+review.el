;;; +review.el --- collect review notes per project  -*- lexical-binding: t; -*-

;;; Commentary:

;; Collect `file:line:column: note' lines in a per-project buffer, for
;; handing over to an LLM to execute.

;;; Code:

(require 'compile)
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
  :group 'convenience
  :keymap +review-mode-map)

(defvar-local +review--project-root nil
  "Root of the project the review buffer belongs to.")
(put '+review--project-root 'permanent-local t)

(defconst +review--location-regexp
  "^\\([^:\n]+\\):\\([0-9]+\\):\\([0-9]+\\):"
  "Regexp matching a location at the start of a review note.")

(defun +review--location-at-point ()
  "Return (FILE LINE COLUMN) if point is on the location of a note."
  (let ((pos (point)))
    (save-excursion
      (beginning-of-line)
      (when (and (looking-at +review--location-regexp)
                 (<= pos (match-end 3)))
        (list (match-string-no-properties 1)
              (string-to-number (match-string-no-properties 2))
              (string-to-number (match-string-no-properties 3)))))))

(defun +review-visit ()
  "Visit the location of the review note at point in another window."
  (interactive)
  (pcase-let ((`(,file ,line ,column)
               (or (+review--location-at-point)
                   (user-error "No location at point"))))
    (pop-to-buffer (find-file-noselect
                    (expand-file-name file +review--project-root))
                   '((display-buffer-reuse-window
                      display-buffer-use-some-window)
                     (inhibit-same-window . t)))
    (goto-char (point-min))
    (forward-line (1- line))
    (move-to-column (1- column))))

(defvar +review-comments-mode-map
  (let ((keymap (make-sparse-keymap)))
    ;; Only shadow RET when on a location, so it's a newline elsewhere.
    (keymap-set keymap "RET"
                `(menu-item "" +review-visit
                            :filter ,(lambda (command)
                                       (when (+review--location-at-point)
                                         command))))
    keymap)
  "Keymap for `+review-comments-mode'.")

;; Same faces as `compilation-mode' uses for its locations.
(defvar +review--font-lock-keywords
  `((,+review--location-regexp
     (1 'compilation-info)
     (2 'compilation-line-number)
     (3 'compilation-column-number)))
  "Font lock keywords for `+review-comments-mode'.")

(define-derived-mode +review-comments-mode text-mode "Review"
  "Major mode for review notes buffers."
  (setq-local font-lock-defaults '(+review--font-lock-keywords)))

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
