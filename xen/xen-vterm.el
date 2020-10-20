;;; xen-vterm.el --- Modifications to vterm-mode.    -*- lexical-binding: t; flycheck-emacs-lisp-load-path: inherit; -*-

;; Copyright (C) 2019  Thomas Fini Hansen

;; Author: Thomas Fini Hansen <xen@xen.dk>
;; Keywords: local

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'face-remap)
(require 'vterm)

(defvar vterm-copy-mode)

(defface xen-vterm-copy-mode-face
  '((t :inherit region))
  "Face remapping for the modeline in vterm-copy-mode."
  :group 'xen)

(defvar-local xen-vterm-copy-mode-cookie nil
  "Cookie for the remapped modeline face.

Used to restore the original mode line face.")

(defun xen-vterm-copy-mode-hook ()
  "Mode hook for vterm-copy-mode. Change the modeline color."
  (if vterm-copy-mode
      (progn (setq xen-vterm-copy-mode-cookie
                   (face-remap-add-relative
                    'mode-line 'xen-vterm-copy-mode-face))
             (hl-line-mode 1))
    (face-remap-remove-relative xen-vterm-copy-mode-cookie)
    (setq xen-vterm-copy-mode-cookie nil)
    (hl-line-mode -1)))

(defun xen-switch-to-shell (&optional buffer-list)
  "Switch to a vterm buffer. Create one or use ivy.

Limit to buffers BUFFER-LIST if supplied."
  (interactive)
  (let* ((buffer-list (or buffer-list (buffer-list)))
         (buffers (seq-filter
                   (lambda (buffer) (and
                                     ;; Major mode is vterm-mode.
                                     (eq 'vterm-mode
                                         (buffer-local-value 'major-mode buffer))
                                     ;; Buffer is not visible.
                                     (not (get-buffer-window buffer t))))
                   buffer-list)))

    (cond
     ((not buffers) (call-interactively 'vterm))
     ;; ((eq 1 (length buffers)) (switch-to-buffer (car buffers)))
     (t (ivy-read "Shell buffer (S-RET for new): "
                  (mapcar #'buffer-name buffers)
                  :matcher #'ivy--switch-buffer-matcher
                  :preselect (buffer-name (other-buffer (current-buffer)))
                  :action #'xen-ivy--switch-vbuffer-action
                  :keymap ivy-switch-buffer-map
                  :caller 'ivy-switch-buffer
                  :update-fn (lambda ()
                               (if (get-buffer (ivy-state-current ivy-last))
                                   (ivy-call))))))))

(defun xen-ivy--switch-vbuffer-action (buffer)
  "Switch to vterm BUFFER.
If BUFFER is an empty string, create a new vterm buffer."
  (if (string-empty-p buffer)
      (vterm)
    (ivy--switch-buffer-action buffer)))

(provide 'xen-vterm)
;;; xen-vterm.el ends here
