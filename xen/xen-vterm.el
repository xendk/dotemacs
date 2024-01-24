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
(defvar xen-switch-to-shell-buffers)

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

(defvar xen-consult--source-vterm-buffer
  `(:name "VTerm"
          :narrow   ?v
          :category buffer
          :face     consult-buffer
          :history  buffer-name-history
          :state    ,#'consult--buffer-state
          :default t
          :hidden t
          :items
          ,(lambda () (consult--buffer-query :sort 'visibility
                                             :mode 'vterm-mode
                                             :as #'buffer-name)))
  "VTerm buffer candidate source for `consult-buffer'.")

;; Same as the above, but not hidden (doesn't work with
;; xen-switch-to-shell when it is).
(defvar xen-consult--source-vterm-buffer2
  `(:name "VTerm"
          :narrow   ?v
          :category buffer
          :face     consult-buffer
          :history  buffer-name-history
          :state    ,#'consult--buffer-state
          :default t
          :items
          ,(lambda () xen-switch-to-shell-buffers))
  "VTerm buffer candidate source for `xen-switch-to-shell'.")
(add-to-list 'consult-buffer-sources 'xen-consult--source-vterm-buffer)

;; idea: Drop trying to piggyback on consult-buffer and just
;; vertico/completing-read. Add options to open new buffer in current
;; dir, or in project root.
(defun xen-switch-to-shell (&optional buffer-list)
  "Switch to a vterm buffer. Create one or use consult-buffer.

Limit to buffers BUFFER-LIST if supplied."
  (interactive)
  (let* ((buffer-list (or buffer-list (buffer-list)))
         ;; All vterm buffers.
         (buffers (seq-filter
                   (lambda (buffer) (and
                                     ;; Major mode is vterm-mode.
                                     (eq 'vterm-mode
                                         (buffer-local-value 'major-mode buffer))))
                   buffer-list))
         ;; Preferably limit to invisible buffers.
         (candidates (seq-filter
                      (lambda (buffer)
                        ;; Buffer is not visible.
                        (not (get-buffer-window buffer t)))
                      buffers)))
    (cond
     ;; No vterm buffers, create one.
     ((not buffers)
      (message "New shell session")
      (call-interactively 'vterm))
     (t
      (let ((consult-buffer-sources '(xen-consult--source-vterm-buffer2))
            ;; Use either candidates or buffers, so we won't present
            ;; an empty list if all are visible.
            (xen-switch-to-shell-buffers (seq-map
                                          (lambda (buffer)
                                            (buffer-name buffer))
                                          (or candidates buffers))))
        (if (> (length xen-switch-to-shell-buffers) 1)
            (consult-buffer)
          ;; One vterm buffer, switch to it.
          (switch-to-buffer (car xen-switch-to-shell-buffers))))))))

(provide 'xen-vterm)
;;; xen-vterm.el ends here
