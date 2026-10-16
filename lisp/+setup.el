;;; +setup.el --- Configure setup.el                     -*- lexical-binding: t; -*-
;;;
;;; Commentary:

;; Install and configure the setup.el configuration macros.

;;; Code:

(declare-function elpaca "elpaca")
(declare-function elpaca-wait "elpaca")
(declare-function setup-define "setup")
(declare-function setup-get "setup")

(defvar setup)
(defvar setup-attributes)
(defvar setup-modifier-list)

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

;; Faces are set in their own theme rather than the `user' theme that
;; `custom-set-faces' uses, as Customize saves everything in the latter
;; to `custom-file'.
(deftheme +setup "Faces set by `+setup-set-faces' and :faces.")
(enable-theme '+setup)

(defun +setup-raise-theme (theme)
  "Re-enable the `+setup' theme when enabling THEME.
Keeps it above other themes (`user' stays on top regardless)."
  ;; Enabling any theme also enables `user', so reacting to it would
  ;; recurse endlessly.
  (unless (memq theme '(+setup user))
    (enable-theme '+setup)))

(add-hook 'enable-theme-functions #'+setup-raise-theme)

(defun +setup-set-faces (&rest entries)
  "Set faces in the `+setup' theme.
ENTRIES are like in `custom-set-faces', but as they're not in the
`user' theme, Customize won't save them to `custom-file'."
  (apply #'custom-theme-set-faces '+setup entries)
  ;; Settings for non-user themes are only recorded, not applied.
  (dolist (entry entries)
    (custom-theme-recalc-face (car entry))))

(setup-define :faces
  (lambda (&rest entries)
    `(+setup-set-faces ,@entries))
  :documentation "Set faces like `custom-set-faces' using `+setup-set-faces'.")

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
         (push renderer (alist-get ',doc devdocs--rendering-functions)))))
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
