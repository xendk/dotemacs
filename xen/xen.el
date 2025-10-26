;;; xen.el --- Assorted functions for my init.el              -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Thomas Fini Hansen

;; Author: Thomas Fini Hansen <xen@claymore>
;; Keywords: local
;; Package-Requires: ((emacs "25") dash smartparens php-mode)
;; Package-Version: 0.0.1
;; Url: https://github.com/xendk/dotemacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is bits and pieces of configuration and handy functions for my
;; setup. Moved here to unclutter init.el.

;;; Code:

;; External variables referenced.
(defvar vterm-copy-mode)
(defvar vterm-shell)

;; External functions.
(declare-function vterm "vterm" (&optional arg))

(defgroup xen nil
  "Personal configuration."
  :group 'emacs)

;; My own prefix command. Bound in init.el.
(defvar xen-map)
(define-prefix-command 'xen-map)

(defvar xen-casing-map)
(define-prefix-command 'xen-casing-map)

(define-key xen-map (kbd "e")
  #'(lambda()
      "Open init.el."
      (interactive)
      (find-file (locate-user-emacs-file "init.el"))))

(define-key xen-map (kbd "t")
  #'(lambda()
      "Open tips file."
      (interactive)
      (find-file (locate-user-emacs-file "tips"))))

(define-key xen-map (kbd "o")
  #'(lambda()
      "Open todo file."
      (interactive)
      (find-file (locate-user-emacs-file "todo"))))

(defun xen-char-syntax ()
  "Show the syntax class of the character following point."
  (interactive)
  (message (char-to-string (char-syntax (char-after)))))

(defun xen-mark-lines ()
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

(defun xen-xml-pretty ()
  "Run xmllint -pretty - on the region."
  (interactive)
  (if (region-active-p) (shell-command-on-region
                         (region-beginning) (region-end)
                         "xmllint -format -" nil t)))

;; Define a FontAwesome face.
(make-face 'xen-font-awesome-face)
(set-face-attribute 'xen-font-awesome-face nil
                    :family "FontAwesome")

;; misc minor modes

(defvar xen-edit-clipboard-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-c C-c") 'xen-edit-clipboard-save)
    keymap)
  "Keymap for xen-edit-clipboard-mode.")

(define-minor-mode xen-edit-clipboard-mode
  "Minor mode for editing clipboard."
  :keymap xen-edit-clipboard-mode-map)

(defun xen-edit-clipboard-save ()
  "Replace the clipboard content with the current buffers content and kill buffer."
  (interactive)
  (kill-new (buffer-string) t)
  (kill-buffer))

(defun xen-edit-clipboard ()
  "Open a new buffer with the contents of the clipboard/top of `kill-ring'."
  (interactive)
  (let ((buffer (generate-new-buffer "*Clipboard*")))
    (with-current-buffer buffer
      (yank)
      (xen-edit-clipboard-mode))
    (switch-to-buffer buffer)))

(defun xen-newline (&optional arg interactive)
  "Insert a newline, handling comments.

Uses `default-indent-new-line' in comments and `newline' otherwise.

After two empty line comments, it'll delete both.

Pass ARG and INTERACTIVE to `newline'."
  (interactive "*P\np")
  (barf-if-buffer-read-only)

  ;; Use syntax table to determine if we're in a comment (gleaned from
  ;; mwim).
  (if (xen-in-comment)
      ;; Remove the comment if we're looking at two empty single line
      ;; comments.
      (let ((empty-comment-start (xen-empty-comment-start)))
        (if empty-comment-start
            (delete-region empty-comment-start (point))
          (xen-default-indent-new-line)))
    (newline arg interactive)))

(defun xen-empty-comment-start ()
  "Return start of empty comment block.

If we're on the second empty single line comment."
  (if comment-start
      (ignore-errors
        (if (xen-comment-is-empty)
            (let ((this-start (xen-in-comment))
                  (trimmed-comment-start (string-trim-right comment-start)))
              (save-excursion
                (forward-line -1)
                (if (xen-in-comment)
                    (let ((start-of-comment (xen-in-comment)))
                      (if (xen-comment-is-empty)
                          ;; We got an empty comment on the previous
                          ;; line, return it, if it matches the
                          ;; comment-start (block comments shouldn't).
                          (progn
                            (goto-char start-of-comment)
                            (if (looking-at-p trimmed-comment-start)
                                start-of-comment))))
                  ;; Return the start of the original if there's no
                  ;; comment on the previous line.
                  (progn
                    (goto-char this-start)
                    (if (looking-at-p trimmed-comment-start)
                        this-start)))))))))

(defun xen-comment-is-empty ()
  "Determine if comment is empty."
  (let ((in-comment (xen-in-comment)))
    (if in-comment
        (save-excursion
          (goto-char in-comment)
          ;; In modes without a unique comment character (php-mode being one
          ;; example), the start comment char(s) doesn't have the comment
          ;; start syntax. So look up the syntax of the first char in the
          ;; comment and skip that class.
          (let ((syntax-class (char-syntax (char-after))))
            (skip-syntax-forward (string syntax-class))
            ;; Skip white-space.
            (skip-syntax-forward "-")
            (or (eobp) (eq (char-syntax (char-after)) ?>)))))))

(defun xen-in-comment ()
  "Return start of comment, or nil if not inside comment."
  (let ((syn (syntax-ppss)))
    (and (nth 4 syn)
         (nth 8 syn))))

(defun xen-default-indent-new-line ()
  "Call `default-indent-new-line', or handles `emacs-lisp-mode' specifically."
  (if (eq major-mode 'emacs-lisp-mode)
      (let (comment-start num-semis)
        (save-excursion
          (goto-char (xen-in-comment))
          (setq comment-start (point))
          (skip-chars-forward ";")
          (setq num-semis (- (point) comment-start)))
        (insert "\n")
        (insert (make-string num-semis ?\;) " "))
    (default-indent-new-line)))

(defun xen-jitter-type (string)
  "Type STRING to buffer."
  (let ((jitter 5))
    (dolist (char (string-to-list string))
      (insert char)
      (sit-for (/
                (+
                 (/ 1.0 (length string))
                 (/ (- (/ jitter 2.0) (random (+ jitter 1))) 100.0)
                 )
                4.0)))))

(defun xen-docker-compose-up ()
  "Run docker-compose up."
  (interactive)
  (let* ((docker-compose-file
          (or (locate-dominating-file default-directory "docker-compose.yml")
              (user-error "No docker-compose.yml found")))
         (default-directory docker-compose-file)
         (buffer-name (concat " *docker compose up "
                              (abbreviate-file-name docker-compose-file) " *")))
    (if (get-buffer buffer-name)
        (pop-to-buffer buffer-name '(display-buffer-reuse-window display-buffer-same-window))
      (let ((vterm-shell "docker compose up"))
        (vterm buffer-name)))))

(provide 'xen)
;;; xen.el ends here
