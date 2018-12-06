;;; xen-swiper.el --- Swiper helper functions        -*- lexical-binding: t; -*-

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

(require 'swiper)

(defvar xen-switch-buffer-history nil
  "History for `xen-switch-buffer'.")

(defun xen-swiper ()
  "Call swiper with region (from BEG to END) as initial-input."
  (interactive)
  (swiper (if (use-region-p)
              (progn
                (deactivate-mark)
                (buffer-substring-no-properties
                 (region-beginning)
                 (region-end))))))

(defun xen-swiper-from-isearch ()
  "Invoke `swiper' from isearch."
  (interactive)
  (let ((query (if isearch-regexp
                   isearch-string
                 (regexp-quote isearch-string))))
    (isearch-exit)
    (swiper query)))

(defun xen-counsel-ag ()
  "Invoke `counsel-ag' with contents of regian."
  (interactive)
  (counsel-ag (buffer-substring-no-properties (region-beginning) (region-end))))

(defun xen-switch-buffer ()
  "Switch to another buffer."
  (interactive)
  (let ((this-command 'ivy-switch-buffer))
    (ivy-read "Switch to buffer: "
              (lambda (string predicate code)
                (delete (buffer-name (current-buffer))
                        (internal-complete-buffer string predicate code)))
              :matcher #'ivy--switch-buffer-matcher
              :preselect (buffer-name (other-buffer (current-buffer)))
              :action #'ivy--switch-buffer-action
              :history xen-switch-buffer-history
              :keymap ivy-switch-buffer-map
              :caller 'ivy-switch-buffer
              ;; Using an lambda rather than raw ivy-call, in order
              ;; to only call it on existing buffers.
              :update-fn (lambda ()
                           (if (get-buffer (ivy-state-current ivy-last))
                               (ivy-call))))))
(provide 'xen-swiper)
;;; xen-swiper.el ends here
