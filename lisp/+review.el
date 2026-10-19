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

;; Line and column are optional, as `+review-delete-backward' trims
;; them. No whitespace in the file name, so a note starting with
;; "Note: " isn't mistaken for a location.
(defconst +review--location-regexp
  "^\\([^: \t\n]+\\)\\(?::\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?\\)?:"
  "Regexp matching a location at the start of a review note.")

(defun +review--location-at-point ()
  "Return (FILE LINE COLUMN) if point is on the location of a note.
LINE and COLUMN are nil if the location doesn't have them."
  (let ((pos (point)))
    (save-excursion
      (beginning-of-line)
      (when (and (looking-at +review--location-regexp)
                 (< pos (match-end 0)))
        (list (match-string-no-properties 1)
              (and (match-beginning 2)
                   (string-to-number (match-string-no-properties 2)))
              (and (match-beginning 3)
                   (string-to-number (match-string-no-properties 3))))))))

(defun +review--after-location-p ()
  "Return non-nil if point is right after a location and its space.
Sets the match data to the location."
  (let ((pos (point)))
    (save-excursion
      (beginning-of-line)
      (and (looking-at (concat +review--location-regexp " "))
           (= pos (match-end 0))
           ;; Removing the whole line would lose the note.
           (or (match-beginning 2) (= pos (line-end-position)))))))

(defun +review-delete-backward ()
  "Make the location before point less precise.
Removes the column, then the line number, then the whole line."
  (interactive)
  (unless (+review--after-location-p)
    (user-error "Not after a location"))
  (cond ((match-beginning 3)
         (delete-region (1- (match-beginning 3)) (match-end 3)))
        ((match-beginning 2)
         (delete-region (1- (match-beginning 2)) (match-end 2)))
        ;; On the last line, take the preceding newline instead, so
        ;; point ends up at the end of the previous note.
        ((and (= (line-end-position) (point-max))
              (> (line-beginning-position) (point-min)))
         (delete-region (1- (line-beginning-position)) (point-max)))
        (t
         (delete-region (line-beginning-position)
                        (min (1+ (line-end-position)) (point-max))))))

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
    (when line
      (goto-char (point-min))
      (forward-line (1- line))
      (move-to-column (1- (or column 1))))))

(defvar +review-comments-mode-map
  (let ((keymap (make-sparse-keymap)))
    ;; Only shadow RET when on a location, so it's a newline elsewhere.
    (keymap-set keymap "RET"
                `(menu-item "" +review-visit
                            :filter ,(lambda (command)
                                       (when (+review--location-at-point)
                                         command))))
    ;; Likewise DEL is only special right after a location.
    (keymap-set keymap "DEL"
                `(menu-item "" +review-delete-backward
                            :filter ,(lambda (command)
                                       (when (+review--after-location-p)
                                         command))))
    (keymap-set keymap "C-c C-c" '+review-bury)
    (keymap-set keymap "C-c C-k" '+review-kill)
    keymap)
  "Keymap for `+review-comments-mode'.")

;; Same faces as `compilation-mode' uses for its locations.
(defvar +review--font-lock-keywords
  `((,+review--location-regexp
     (1 'compilation-info)
     (2 'compilation-line-number nil t)
     (3 'compilation-column-number nil t)))
  "Font lock keywords for `+review-comments-mode'.")

(define-derived-mode +review-comments-mode text-mode "Review"
  "Major mode for review notes buffers."
  (setq-local font-lock-defaults '(+review--font-lock-keywords)))

(defun +review--quit (kill)
  "Remove the review window, and bury the buffer, or KILL it."
  (let ((buffer (current-buffer)))
    ;; The sole window can't be deleted, so just switch it away.
    (if (one-window-p)
        (if kill (kill-buffer) (bury-buffer))
      (delete-window)
      (if kill (kill-buffer buffer) (bury-buffer buffer)))))

(defun +review-bury ()
  "Bury the review buffer and remove its window."
  (interactive)
  (+review--quit nil))

(defun +review-kill ()
  "Kill the review buffer and remove its window.
Asks for confirmation if the buffer has any notes."
  (interactive)
  (when (or (string-blank-p (buffer-string))
            (yes-or-no-p "Kill review buffer and lose its notes? "))
    (+review--quit t)))

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
