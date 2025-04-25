;;; +vterm.el --- vterm additions                    -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'face-remap)

(declare-function consult--buffer-state "consult")
(declare-function consult--buffer-query "consult")
(declare-function consult-buffer "consult")

(defvar consult-buffer-sources)
(defvar vterm-copy-mode)

(defface +vterm-copy-mode-face
  '((t :inherit region))
  "Face remapping for the modeline in vterm-copy-mode."
  :group 'xen)

(defvar-local +vterm-copy-mode-cookie nil
  "Cookie for the remapped modeline face.

Used to restore the original mode line face.")

(defun +vterm-copy-mode-hook ()
  "Mode hook for vterm-copy-mode. Change the modeline color."
  (if vterm-copy-mode
      (progn (setq +vterm-copy-mode-cookie
                   (face-remap-add-relative
                    'mode-line '+vterm-copy-mode-face))
             (hl-line-mode 1))
    (face-remap-remove-relative +vterm-copy-mode-cookie)
    (setq +vterm-copy-mode-cookie nil)
    (hl-line-mode -1)))


;; Buffer switching
(defvar +vterm-switch-to-shell-buffers)

(defvar +consult--source-vterm-buffer
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

(add-to-list 'consult-buffer-sources '+consult--source-vterm-buffer)

;; Same as the above, but not hidden (doesn't work with
;; +vterm-switch-to-shell when it is).
(defvar +consult--source-vterm-buffer2
  `(:name "VTerm"
          :narrow   ?v
          :category buffer
          :face     consult-buffer
          :history  buffer-name-history
          :state    ,#'consult--buffer-state
          :default t
          :items
          ,(lambda () +vterm-switch-to-shell-buffers))
  "VTerm buffer candidate source for `+vterm-switch-to-shell'.")

;; IDEA: Drop trying to piggyback on consult-buffer and just
;; vertico/completing-read. Add options to open new buffer in current
;; dir, or in project root.
(defun +vterm-switch-to-shell (&optional buffer-list)
  "Switch to a vterm buffer. Create one or use consult-buffer.

Limit to buffers BUFFER-LIST if supplied."
  (interactive)
  (let* ((buffer-list (or buffer-list (buffer-list)))
         ;; All vterm buffers.
         (buffers (seq-filter
                   (lambda (buffer)
                     (and
                      ;; Major mode is vterm-mode.
                      (eq 'vterm-mode
                          (buffer-local-value 'major-mode buffer))
                      ;; And not hidden. Only my docker-compose
                      ;; buffers hit this, but until I get them to use
                      ;; compile-mode...
                      (not (string-match "\\` " (buffer-name buffer)))))
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
      (let ((consult-buffer-sources '(+consult--source-vterm-buffer2))
            ;; Use either candidates or buffers, so we won't present
            ;; an empty list if all are visible.
            (+vterm-switch-to-shell-buffers (seq-map
                                             (lambda (buffer)
                                               (buffer-name buffer))
                                             (or candidates buffers))))
        (if (> (length +vterm-switch-to-shell-buffers) 1)
            (consult-buffer)
          ;; One vterm buffer, switch to it.
          (switch-to-buffer (car +vterm-switch-to-shell-buffers))))))))

(provide '+vterm)
;;; +vterm.el ends here
