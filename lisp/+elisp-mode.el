;;; +elisp-mode.el --- elisp-mode additions          -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(defun +elisp-mode-test-switch ()
  "Switch between Lisp and test files in .config/emacs."
  (interactive)
  (when buffer-file-name
    (let ((base (string-remove-prefix (expand-file-name user-emacs-directory) buffer-file-name)))
      (when-let (new (cond
                      ((string-prefix-p "lisp/" base) (string-replace "lisp/" "tests/"base ))
                      ((string-prefix-p "tests/" base) (string-replace "tests/" "lisp/"base ))
                      (t nil)))
        (find-file (concat (expand-file-name user-emacs-directory) new))))))

(provide '+elisp-mode)
;;; +elisp-mode.el ends here
