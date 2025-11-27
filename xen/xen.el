;;; xen.el --- Assorted functions for my init.el              -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Thomas Fini Hansen

;; Author: Thomas Fini Hansen <xen@claymore>
;; Keywords: local
;; Package-Requires: ((emacs "25") dash smartparens php-mode)
;; Package-Version: 0.0.1
;; Url: https://github.com/xendk/dotemacs

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

;; This is bits and pieces of configuration and handy functions for my
;; setup. Moved here to unclutter init.el.

;;; Code:

;; External variables referenced.
(defvar vterm-copy-mode)
(defvar vterm-shell)

;; External functions.
(declare-function vterm "vterm" (&optional arg))

(defun xen-char-syntax ()
  "Show the syntax class of the character following point."
  (interactive)
  (message (char-to-string (char-syntax (char-after)))))

(defun xen-xml-pretty ()
  "Run xmllint -pretty - on the region."
  (interactive)
  (if (region-active-p) (shell-command-on-region
                         (region-beginning) (region-end)
                         "xmllint -format -" nil t)))

(defun xen-jitter-type (string)
  "Type STRING to buffer, in a semi-natural way.

Insert string character by character, but add a random delay to make it
less machine-like."
  (let ((jitter 5))
    (dolist (char (string-to-list string))
      (insert char)
      (sit-for (/
                (+
                 (/ 1.0 (length string))
                 (/ (- (/ jitter 2.0) (random (+ jitter 1))) 100.0)
                 )
                4.0)))))

(provide 'xen)
;;; xen.el ends here
