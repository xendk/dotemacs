;;; xen-term-mode.el --- Term mode helper functions  -*- lexical-binding: t; -*-

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

(require 'face-remap)

(defface xen-term-line-mode-face
  '((t :inherit region))
  "Face remapping for the modeline in term-line-mode."
  :group 'xen)

(defvar-local xen-term-mode-position nil
  "Saved position of term-char-mode.")

(defvar-local xen-term-mode-line-cookie nil
  "Cookie for the remapped modeline face.

Used to restore the original mode line face.")

(defun xen-term-line-mode-advice ()
  "Save current point. And set mode-line color."
  (setq xen-term-mode-position (point))
  (setq xen-term-mode-line-cookie (face-remap-add-relative 'mode-line 'xen-term-line-mode-face)))
(advice-add 'term-line-mode :before #'xen-term-line-mode-advice)

(defun xen-term-char-mode-advice ()
  "Restore saved point (if set). And reset mode-line color."
  (when xen-term-mode-position
    (goto-char xen-term-mode-position))
  (face-remap-remove-relative xen-term-mode-line-cookie)
  (setq xen-term-mode-line-cookie nil))
(advice-add 'term-char-mode :before #'xen-term-char-mode-advice)

(provide 'xen-term-mode)
;;; xen-term-mode.el ends here
