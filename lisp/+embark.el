;;; +embark.el --- embark additions                  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun +embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-argument" (cdr binding))))))))

(defun +embark-hide-which-key-indicator (fn &rest args)
  "Hide the which-key indicator immediately when using the completing-read prompter."
  (which-key--hide-popup-ignore-command)
  (let ((embark-indicators
         (remq #'embark-which-key-indicator embark-indicators)))
    (apply fn args)))

(advice-add #'embark-completing-read-prompter
            :around #'+embark-hide-which-key-indicator)

;; TODO: Should be moved to google-this.
(defun +embark-google-region ()
  "Google current region."
  (interactive)
  (google-this-region nil t))

(defvar sort-fold-case)
(defun +embark-sort-lines-caseless ()
  "Sort selected lines caselessly."
  (interactive)
  (let ((sort-fold-case t))
    (call-interactively 'sort-lines)))

(provide '+embark)
;;; +embark.el ends here
