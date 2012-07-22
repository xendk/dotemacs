;;; drupal-mode.el --- major mode for Drupal coding

;;;###autoload
(define-derived-mode drupal-mode php-mode "Drupal"
  "Major mode for Drupal coding.\n\n\\{drupal-mode-map}"
  (setq c-basic-offset 2)
  (setq indent-tabs-mode nil)
  (setq fill-column 78)
  (setq show-trailing-whitespace t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
                                        ; (c-set-offset 'case-label 2)
  (c-set-offset 'case-label '+)
  (c-set-offset 'arglist-close 0)
                                        ;(c-set-offset 'arglist-intro 2)
  (c-set-offset 'arglist-intro '+) ; for FAPI arrays and DBTNG
  (c-set-offset 'arglist-cont-nonempty 'c-lineup-math) ; for DBTNG fields and values
  ;; (run-hooks 'drupal-mode-hook)
)

(define-key drupal-mode-map (kbd "C-c d h") 'drupal-hook-insert)
(define-key drupal-mode-map (kbd "C-c d f") 'drupal-form-insert)


(defun drupal-module-name ()
  "Return name of module"
  (car (split-string (buffer-name) "\\.")))

(defun drupal-hook-insert (name)
  "Insert template for a drupal hook"
  (interactive "MEnter hook name: ")
  (insert (concat "/**\n* Implements hook_" name "().\n */\nfunction " (drupal-module-name) "_" name "() {\n\n}\n")))

(defun drupal-form-insert (name)
  "Insert templat for a drupal form"
  (interactive "MEnter form name: ")
  (insert (concat "/**\n * Form builder for .\n */\nfunction " (drupal-module-name) "_" name "($form_state) {\n\n}\n"))
  (insert (concat "\n\n/**\n * Form validation handler for " (drupal-module-name) "_" name "().\n */\nfunction " (drupal-module-name) "_" name "_validate($form, &$form_state) {\n\n}"))
  (insert (concat "\n\n/**\n * Form submission handler for " (drupal-module-name) "_" name "().\n */\nfunction " (drupal-module-name) "_" name "_submit($form, &$form_state) {\n\n}")))

(provide 'drupal-mode)
