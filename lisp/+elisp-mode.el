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

(with-eval-after-load 'autoinsert
  (defvar auto-insert-alist)

  (add-to-list 'auto-insert-alist
                ;; Template for my + files. To avoid the verbose .el file default.
                '((".config/emacs/lisp/.*\\.el\\'" . "Emacs Lisp custom configuration")
                  "Short description: " ";;; "
                  (file-name-nondirectory (buffer-file-name)) " --- " str
                  (make-string (max 2 (- 80 (current-column) 27)) 32)
                  "-*- lexical-binding: t; -*-" '(setq lexical-binding t) "

;;; Commentary:

;; "
               _ "

;;; Code:



(provide '"
               (file-name-base (buffer-file-name)) ")
;;; "
               (file-name-nondirectory (buffer-file-name)) " ends here
"))

(add-to-list 'auto-insert-alist
             ;; Template for test files in .config/emacs.
             '((".config/emacs/tests/.*\\.el\\'" . "Emacs Lisp custom configuration")
               nil ";;; -*- lexical-binding: t; -*-

(require 'buttercup)

"))
)

(provide '+elisp-mode)
;;; +elisp-mode.el ends here
