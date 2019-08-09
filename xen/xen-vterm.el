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

(provide 'xen-vterm)
;;; xen-vterm.el ends here
