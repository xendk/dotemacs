;;; +edit-clipboard.el --- quickly edit the clipboard  -*- lexical-binding: t; -*-

;;; Commentary:

;; Quickly edit the clipboard using a temporary buffer.

;;; Code:

(defvar +edit-clipboard-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-c C-c") '+edit-clipboard-save)
    keymap)
  "Keymap for +edit-clipboard-mode.")

(define-minor-mode +edit-clipboard-mode
  "Minor mode for editing clipboard."
  :keymap +edit-clipboard-mode-map)

(defun +edit-clipboard-save ()
  "Replace the clipboard content with the current buffers content and kill buffer."
  (interactive)
  (kill-new (buffer-string) t)
  (kill-buffer))

(defun +edit-clipboard ()
  "Open a new buffer with the contents of the clipboard/top of `kill-ring'."
  (interactive)
  (let ((buffer (generate-new-buffer "*Clipboard*")))
    (with-current-buffer buffer
      (yank)
      (+edit-clipboard-mode))
    (switch-to-buffer buffer)))


(provide '+edit-clipboard)
;;; +edit-clipboard.el ends here
