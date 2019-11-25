;;; xen-smartparens.el --- Smartparens helper functions  -*- lexical-binding: t; -*-

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

;;

;;; Code:

(require 'smartparens)

(defvar xen-delete-char-disabled)

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

(defun xen-delete-char-advice (orig-fun n &optional kill-flag)
  "Advice for delete char.  ORIG-FUN is the overriden function. Use N and ignore KILL-FLAG."
  (if (not (boundp 'xen-delete-char-disabled))
      (let ((xen-delete-char-disabled t))
        (save-match-data (progn
                           (if (not (xen-paired-delete (> 0 n) (abs n)))
                               (funcall orig-fun n kill-flag)))))
    (funcall orig-fun n kill-flag)))
(advice-add 'delete-char :around #'xen-delete-char-advice)

(defun xen-sp-insert-pair-advice (orig-fun &rest args)
  "Advice to disable paired delete in sp-insert-pair/sp-skip-closing-pair.  Call ORIG-FUN with ARGS."
  (let ((xen-delete-char-disabled t))
    (apply orig-fun args)))
(advice-add 'sp-insert-pair :around #'xen-sp-insert-pair-advice)
(advice-add 'sp-skip-closing-pair :around #'xen-sp-insert-pair-advice)

(put 'xen-paired-delete 'delete-selection 'supersede)

(provide 'xen-smartparens)
;;; xen-smartparens.el ends here
