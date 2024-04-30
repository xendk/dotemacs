;;; +setup.el --- Configure setup.el                     -*- lexical-binding: t; -*-
;;;
;;; Commentary:

;; Install and configure the setup.el configuration macros.

;;; Code:

(declare-function elpaca "elpaca")
(elpaca setup
  (require 'setup))

;; Block until elpaca has installed it.
(elpaca-wait)

(defun +setup-wrap-to-install-package (body _name)
  "Wrap BODY in an `elpaca' block if necessary.
The body is wrapped in an `elpaca' block if `setup-attributes'
contains an alist with the key `elpaca'."
  (if (assq 'elpaca setup-attributes)
      `(elpaca ,(cdr (assq 'elpaca setup-attributes)) ,@(macroexp-unprogn body))
    body))

;; Add the wrapper function
(add-to-list 'setup-modifier-list #'+setup-wrap-to-install-package)

(setup-define :elpaca
  (lambda (order &rest recipe)
    (push (cond
           ((eq order t) `(elpaca . ,(setup-get 'feature)))
           ((eq order nil) '(elpaca . nil))
           (`(elpaca . (,(setup-get 'feature) ,order ,@recipe))))
          setup-attributes)
    ;; If the macro wouldn't return nil, it would try to insert the result of
    ;; `push' which is the new value of the modified list. As this value usually
    ;; cannot be evaluated, it is better to return nil which the byte compiler
    ;; would optimize away anyway.
    nil)
  :documentation "Install ORDER with `elpaca'.
The ORDER can be used to deduce the feature context."
  :shorthand #'cadr)

(setup-define :face
  (lambda (face spec)
    `(custom-set-faces (list ',face (list (list t ,spec)) 'now "Customized by `setup'.")))
  :documentation "Customize FACE with SPEC using `custom-set-faces'."
  :repeatable t)

(setup-define :load-from
  (lambda (path)
    `(let ((path* (expand-file-name ,path)))
       (if (file-exists-p path*)
           (add-to-list 'load-path path*)
         ,(setup-quit))))
  :documentation "Add PATH to load path.
This macro can be used as NAME, and it will replace itself with
the nondirectory part of PATH.
If PATH does not exist, abort the evaluation."
  :shorthand (lambda (args)
               (intern
                (file-name-nondirectory
                 (directory-file-name (cadr args))))))

;; Alternative to :file-match
(setup-define :files
  (lambda (glob)
    `(add-to-list 'auto-mode-alist (cons ,(wildcard-to-regexp glob) ',(setup-get 'mode))))
  :documentation "Associate the current mode with files that match GLOB."
  :debug '(form)
  :repeatable t)

(provide '+setup)
;;; +setup.el ends here

;; Local Variables:
;; flycheck-emacs-lisp-load-path: 'inherit
;; End:
