;;; +hl-line.el --- hl-line additions                -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; TODO Use `global-hl-line-modes' instead.
(defun +hl-line-mode ()
  "Enable `hl-line-mode' unless in minibuffer or vterm-mode."
  (unless (or (minibufferp)
              ;; Let xen-vterm handle hl-line-mode toggling
              ;; in vterm buffers.
              (eq major-mode 'vterm-mode))
    (hl-line-mode)))

(provide '+hl-line)
;;; +hl-line.el ends here
