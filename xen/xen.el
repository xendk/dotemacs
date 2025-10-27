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

(defun xen-jitter-type (string)
  "Type STRING to buffer, in a semi-natural way.

Insert string character by character, but add a random delay to make it
less machine-like."
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
