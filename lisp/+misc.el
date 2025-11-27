;;; +misc.el --- misc stuff created over the years   -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(defun +char-syntax ()
  "Show the syntax class of the character following point."
  (interactive)
  (message (char-to-string (char-syntax (char-after)))))

(defun +xml-pretty ()
  "Run xmllint -pretty - on the region."
  (interactive)
  (when (region-active-p)
    (shell-command-on-region
     (region-beginning) (region-end)
     "xmllint -format -" nil t)))

(defun +jitter-type (string)
  "Type STRING to buffer, in a semi-natural way.

Insert string character by character, but add a random delay to make it
less machine-like."
  (let ((jitter 5))
    (dolist (char (string-to-list string))
      (insert char)
      (sit-for (/ (+ (/ 1.0 (length string))
                     (/ (- (/ jitter 2.0) (random (+ jitter 1))) 100.0))
                  4.0)))))

(provide '+misc)
;;; +misc.el ends here
