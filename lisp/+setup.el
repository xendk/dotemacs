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

(setup-define :bind-prefix
  (lambda (key prefix)
    `(define-key ,(setup-get 'map) ,key ,prefix))
  :documentation "Bind KEY to PREFIX in current map."
  :after-loaded t
  :debug '(form sexp)
  :ensure '(kbd nil)
  :repeatable t)

(setup-define :devdoc
  (lambda (doc &rest docs)
    `(:local-set devdocs-current-docs ',(cons doc docs)))
  :documentation "Set DOC for current mode."
  :debug '(form)
  :after-loaded t)

(setup-define :devdoc-tag-render
  (lambda (doc &rest renders)
    `(with-eval-after-load 'devdocs
       (dolist (renderer ',renders)
         (push renderer (alist-get ',doc devdocs-extra-rendering-functions)))))
  :documentation "Add element renders for DOC to RENDERS.
RENDERS is a list of (tag . function) cons."
  :debug '(form))

(setup-define :magic
  (lambda (magic &optional mode)
    `(add-to-list 'magic-mode-alist
                  '(,magic . ,(or mode (setup-get 'mode)))))
  :documentation "Use current (or provided) mode for files containing MAGIC."
  :debug '(form))

(provide '+setup)
;;; +setup.el ends here
