;;; xen-paired-delete.el --- Delete pairs mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Thomas Fini Hansen

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; todo: figure out how this messes with visual-regexp. It shouldn't
;; trigger when vr is doing its replacements.

;;; Code:

(require 'smartparens)

(defvar xen-paired-delete-char-disabled)

(defcustom xen-paired-delete-ignore-modes-list '(
                                                 minibuffer-inactive-mode
                                                 vterm-mode
                                                 )
  "Modes where xen-paired-delete mode is inactive if allowed globally."
  :type '(repeat symbol)
  :group 'xen-paired-delete)

(defun xen-paired-delete (backwards-p &optional arg)
  "Deletes the matching pair if deleting a pair.

BACKWARDS-P indicates whether we're deleting forwards or backwards and we'll
only work when ARG is 1 or the region is not active."
  (when (and (= arg 1)
             (bound-and-true-p smartparens-mode)
             (not (use-region-p)))
    ;; Set sp-navigate-consider-symbols to only get balanced symbols.
    ;; Else it would consider `//' a pair in Crystal (which it is),
    ;; but sp-unwrap-sexp won't work on it, but rather unwrap the next
    ;; pair it finds.
    (let* ((sp-navigate-consider-symbols nil)
           (ok (sp-get-thing backwards-p)))
      (when ok
        (sp-get ok
          (cond
           ((and backwards-p (or (= (point) :beg-in) (= (point) :end)))
            (sp-backward-unwrap-sexp))
           ((and (not backwards-p) (or (= (point) :beg) (= (point) :end-in)))
            (sp-unwrap-sexp))
           (t nil)))))))

(defun xen-paired-delete-delete-char-advice (orig-fun n &optional kill-flag)
  "Advice for delete char.  ORIG-FUN is the overridden function. Pass N and KILL-FLAG to original."
  (if (and xen-paired-delete-mode
           (not
            (boundp 'xen-paired-delete-char-disabled)))
      (let ((xen-paired-delete-char-disabled t))
        (save-match-data (progn
                           (if (not (xen-paired-delete (> 0 n) (abs n)))
                               (funcall orig-fun n kill-flag)))))
    (funcall orig-fun n kill-flag)))

(defun xen-paired-delete-disable-advice (orig-fun &rest args)
  "Advice to disable paired delete in for some functions.  Call ORIG-FUN with ARGS."
  (let ((xen-paired-delete-char-disabled t))
    (apply orig-fun args)))

(put 'xen-paired-delete 'delete-selection 'supersede)

(define-minor-mode xen-paired-delete-mode
  "Toggle paired delete mode."
  :init-value nil
  (if xen-paired-delete-mode
      (progn
        (advice-add 'delete-char :around #'xen-paired-delete-delete-char-advice)
        (advice-add 'sp-insert-pair :around #'xen-paired-delete-disable-advice)
        (advice-add 'sp-skip-closing-pair :around #'xen-paired-delete-disable-advice))
    (advice-remove 'delete-char #'xen-paired-delete-delete-char-advice)
    (advice-remove 'sp-insert-pair #'xen-paired-delete-disable-advice)
    (advice-remove 'sp-skip-closing-pair #'xen-paired-delete-disable-advice))
  )

(defun turn-on-xen-paired-delete-mode ()
  "Turn on `xen-paired-delete-mode'.

This function is used to turn on `global-xen-paired-delete-mode'.

By default `global-xen-paired-delete-mode' ignores buffers with
`mode-class' set to special, but only if they are also not comint
buffers.

Additionally, buffers on `xen-paired-delete-ignore-modes-list' are ignored.

You can still turn on xen-paired-delete-mode in these mode manually (or
in mode's startup-hook etc.) by calling `xen-paired-delete-mode'."
  (interactive)
  (unless (or (member major-mode xen-paired-delete-ignore-modes-list)
              (and (not (derived-mode-p 'comint-mode))
                   (eq (get major-mode 'mode-class) 'special)))
    (xen-paired-delete-mode t)))

(define-globalized-minor-mode global-xen-paired-delete-mode
  xen-paired-delete-mode
  turn-on-xen-paired-delete-mode)

(provide 'xen-paired-delete)
;;; xen-paired-delete.el ends here
