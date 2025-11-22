;;; +auto-insert.el --- auto-insert additions        -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

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

(provide '+auto-insert)
;;; +auto-insert.el ends here
