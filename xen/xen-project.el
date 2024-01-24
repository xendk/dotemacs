;;; xen-project.el --- Project helper functions  -*- lexical-binding: t; -*-

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

(require 'project)
(require 'xen-vterm)

(defun xen-project-switch-to-shell ()
  "Switch to shell buffer in project. Use ivy if multiple buffers."
  (interactive)
  (if (project-current)
      ;; Switch to project root in case we create a new shell buffer.
      (let ((default-directory (project-root (project-current t))))
        (xen-switch-to-shell (project-buffers (project-current))))
    (message "No project.")))

(defun xen-project-vterm ()
  "Start new vterm session in project root."
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (vterm)))

(provide 'xen-project)
;;; xen-project.el ends here
