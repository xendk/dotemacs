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

(defun xen-paired-delete (backwards-p &optional arg)
  "Deletes the matching pair if deleting a pair.

BACKWARDS-P indicates whether we're deleting forwards or backwards and we'll
only work when ARG is 1 or the region is not active."
  (when (and (= arg 1)
             (bound-and-true-p smartparens-mode)
             (not (use-region-p)))
    (let ((ok (sp-get-thing backwards-p)))
      (when ok
        (sp-get ok (progn
                     ;; If either open or close is empty, bomb
                     ;; out. This is the case for symbols, and
                     ;; anyway it doesn't make sense. Look into
                     ;; sp-navigate-consider-symbols for
                     ;; sp-get-thing.
                     (unless (or (equal :op "") (equal :cl ""))
                       (cond
                        ((and backwards-p (or (= (point) :beg-in) (= (point) :end)))
                         (sp-backward-unwrap-sexp))
                        ((and (not backwards-p) (or (= (point) :beg) (= (point) :end-in)))
                         (sp-unwrap-sexp))
                        (t nil)))))))))

(defun xen-paired-delete-delete-char-advice (orig-fun n &optional kill-flag)
  "Advice for delete char.  ORIG-FUN is the overriden function. Pass N and KILL-FLAG to original."
  (if (not (boundp 'xen-paired-delete-char-disabled))
      (let ((xen-paired-delete-char-disabled t))
        (save-match-data (progn
                           (if (not (xen-paired-delete (> 0 n) (abs n)))
                               (funcall orig-fun n kill-flag)))))
    (funcall orig-fun n kill-flag)))
;; (advice-add 'delete-char :around #'xen-paired-delete-delete-char-advice)

(defun xen-paired-delet-sp-insert-pair-advice (orig-fun &rest args)
  "Advice to disable paired delete in sp-insert-pair/sp-skip-closing-pair.  Call ORIG-FUN with ARGS."
  (let ((xen-paired-delete-char-disabled t))
    (apply orig-fun args)))
;; (advice-add 'sp-insert-pair :around #'xen-paired-delet-sp-insert-pair-advice)
;; (advice-add 'sp-skip-closing-pair :around #'xen-paired-delet-sp-insert-pair-advice)

(put 'xen-paired-delete 'delete-selection 'supersede)

(define-minor-mode xen-paired-delete-mode
  "Toggle paired delete mode."
  :init-value nil
  (if xen-paired-delete-mode
      (progn
        (advice-add 'delete-char :around #'xen-paired-delete-delete-char-advice)
        (advice-add 'sp-insert-pair :around #'xen-paired-delet-sp-insert-pair-advice)
        (advice-add 'sp-skip-closing-pair :around #'xen-paired-delet-sp-insert-pair-advice))
    (advice-remove 'delete-char #'xen-paired-delete-delete-char-advice)
    (advice-remove 'sp-insert-pair #'xen-paired-delet-sp-insert-pair-advice)
    (advice-remove 'sp-skip-closing-pair #'xen-paired-delet-sp-insert-pair-advice)))

(define-globalized-minor-mode global-xen-paired-delete-mode
  xen-paired-delete-mode
  xen-paired-delete-mode)

(provide 'xen-paired-delete)
;;; xen-paired-delete.el ends here
