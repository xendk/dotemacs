;;; xen-company.el --- Company helper functions      -*- lexical-binding: t; -*-

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

(require 'company)
(require 'company-dabbrev)
(require 'company-dabbrev-code)

(defun company-dabbrev-code-xen (command &optional arg &rest ignored)
  "A dabbrev-like `company-mode' backend for code.

Works like company-dabbrev-code with
`company-dabbrev-code-everywhere' nil in code but t in comments.

COMMAND, ARG and IGNORED is the arguments passed by company."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-dabbrev-code))
    (prefix (and (or (eq t company-dabbrev-code-modes)
                     (apply #'derived-mode-p company-dabbrev-code-modes))
                 (or (company-grab-symbol) 'stop)))
    (candidates (let ((case-fold-search company-dabbrev-code-ignore-case))
                  (company-dabbrev--search
                   (company-dabbrev-code--make-regexp arg)
                   company-dabbrev-code-time-limit
                   (pcase company-dabbrev-code-other-buffers
                     (`t (list major-mode))
                     (`code company-dabbrev-code-modes)
                     (`all `all))
                   (not (company-in-string-or-comment)))))
    (ignore-case company-dabbrev-code-ignore-case)
    (duplicates t)))


(provide 'xen-company)
;;; xen-company.el ends here
