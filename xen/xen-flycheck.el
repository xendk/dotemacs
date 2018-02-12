;;; xen-flycheck.el --- Flycheck helper functions.             -*- lexical-binding: t; -*-

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

(require 'flycheck)

(defun xen-flycheck-mode-line-status-text (&optional status)
  "Get a text describing STATUS for use in the mode line.

STATUS defaults to `flycheck-last-status-change' if omitted or
nil.

This is my own version using FontAwesome icons."
  (let ((text (pcase (or status flycheck-last-status-change)
                (`not-checked "")
                (`no-checker (propertize (concat [#xF141])
                                         'face 'xen-font-awesome-face))
                (`running (propertize (concat [#xF110])
                                      'face 'xen-font-awesome-face))
                (`errored (propertize (concat [#xF12A])
                                      'face 'xen-font-awesome-face))
                (`finished
                 (let-alist (flycheck-count-errors flycheck-current-errors)
                   (if (or .error .warning)
                       (list (propertize (concat [#xF00D])
                                           'face 'xen-font-awesome-face)
                                (format "%s/%s" (or .error 0) (or .warning 0)))
                     (propertize (concat [#xF00C])
                                 'face 'xen-font-awesome-face))))
                (`interrupted (propertize (concat [#xF127])
                                          'face 'xen-font-awesome-face))
                (`suspicious (propertize (concat [#xF128])
                                         'face 'xen-font-awesome-face)))))
    (list " " text)))

(provide 'xen-flycheck)
;;; xen-flycheck.el ends here
