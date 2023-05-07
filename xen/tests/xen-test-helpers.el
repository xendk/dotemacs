;;; xen-test-helpers.el --- Helper functions for tests.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Thomas Fini Hansen

;; Author: Thomas Fini Hansen <xen@xen.dk>
;; Keywords:

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

;; Thanks to Fuco1 for much inspiration and outright copying of.
;; See
;; https://github.com/Fuco1/.emacs.d/blob/master/tests/my-test-helper.el
;; for more.

;;; Code:

(defmacro xen-test-with-temp-buffer (initial initform &rest forms)
  "Setup a new buffer, then run FORMS.

First, INITFORM are run in the newly created buffer.

Then INITIAL is inserted (it is expected to evaluate to string).
If INITIAL contains | put point there as the initial
position (the character is then removed).  If it contains M, put
mark there (the character is then removed).

Finally, FORMS are run."
  (declare (indent 2)
           (debug (form form body)))
  `(save-window-excursion
     (let ((case-fold-search nil))
       (with-temp-buffer
         (set-window-buffer (selected-window) (current-buffer))
         (set-input-method nil)
         ,initform
         (insert ,initial)
         (goto-char (point-min))
         (let ((case-fold-search nil))
           (when (search-forward "M" nil t)
             (delete-char -1)
             (set-mark (point))
             (activate-mark))
           (goto-char (point-min))
           (when (search-forward "|" nil t)
             (delete-char -1)))
         ,@forms))))

(defmacro xen-test-with-temp-php-buffer (initial &rest forms)
  "Setup a new `php-mode' buffer.

See xen-test-with-temp-buffer."
  (declare (indent 1)
           (debug (form body)))
  `(xen-test-with-temp-buffer ,initial
                              (php-mode)
                              (smartparens-mode)
                              (sp-local-pair 'php-mode "/*" "*/" :post-handlers '((xen-php-handle-docstring "*")))
                              ,@forms))

(defmacro xen-test-with-temp-elisp-buffer (initial &rest forms)
  "Setup a new `emacs-lisp-mode' buffer.

See xen-test-with-temp-buffer."
  (declare (indent 1)
           (debug (form body)))
  `(xen-test-with-temp-buffer ,initial
                              (emacs-lisp-mode)
                              ,@forms))

(defun xen-expect-buffer-equals (result)
  "Compare buffer to RESULT.

RESULT is a string which should equal the result of
`buffer-string' called in the current buffer.

If RESULT contains |, this represents the position of `point' and
should match.

If RESULT contains M, this represents the position of `mark' and
should match."
  (let ((case-fold-search nil))
    (expect (buffer-string) :to-equal (replace-regexp-in-string "[|M]" "" result))
    (when (string-match-p "|" result)
      (expect (1+ (string-match-p
                   "|" (replace-regexp-in-string "[M]" "" result)))
              :to-be (point)))
    (when (string-match-p "M" result)
      (expect (1+ (string-match-p
                   "M" (replace-regexp-in-string "[|]" "" result)))
              :to-be (mark)))))


  (provide 'xen-test-helpers)
;;; xen-test-helpers.el ends here
