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
           (`(elpaca . (,order ,@recipe))))
          setup-attributes)
    ;; If the macro wouldn't return nil, it would try to insert the result of
    ;; `push' which is the new value of the modified list. As this value usually
    ;; cannot be evaluated, it is better to return nil which the byte compiler
    ;; would optimize away anyway.
    nil)
  :documentation "Install ORDER with `elpaca'.
The ORDER can be used to deduce the feature context."
  :shorthand #'cadr)

(setup-define :hide-mode
  (lambda (&optional mode)
    (let* ((mode (or mode (setup-get 'mode)))
           (mode (if (string-match-p "-mode\\'" (symbol-name mode))
                     mode
                   (intern (format "%s-mode" mode)))))
      `(setq minor-mode-alist
             (delq (assq ',mode minor-mode-alist)
                   minor-mode-alist))))
  :documentation "Hide the mode-line lighter of the current mode.
Alternatively, MODE can be specified manually, and override the
current mode."
  :after-loaded t)

(setup-define :face
  (lambda (face spec)
    `(custom-set-faces (list ,face (list (list t ,spec)) 'now "Customized by `setup'.")))
  :documentation "Customize FACE with SPEC using `custom-set-faces'."
  :repeatable t)

(provide '+setup)
;;; +setup.el ends here

;; Local Variables:
;; flycheck-emacs-lisp-load-path: 'inherit
;; End: