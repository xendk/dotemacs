;;; +global-override-map.el --- One map to rule them all  -*- lexical-binding: t; -*-

;;; Commentary:

;; A keymap that'll override all other (well, other
;; `emulation-mode-map-alists' maps notwithstanding). This was
;; pilfered from how bind-key* does it.

;;; Code:

(defvar +global-override-map (make-keymap)
  "Keymap for `override-global-mode'.")

(define-minor-mode +global-override-mode
  "A minor mode for allowing keybindings to override other modes.
The main purpose of this mode is to simplify bindings keys in
such a way that they take precedence over other modes.

To achieve this, the keymap `+global-override-map' is added to
`emulation-mode-map-alists', which makes it take precedence over
global maps, major-mode maps and keymaps in
`minor-mode-map-alist'."
  :init-value t
  :lighter "")

;; The maps in `emulation-mode-map-alists' take precedence over
;; global, major-mode and minor-mode maps.
(add-to-list 'emulation-mode-map-alists
             `((+global-override-mode . ,+global-override-map)))

(setup-define :global-override
  (lambda (key command)
    `(define-key +global-override-map ,key ,command))
  :documentation "Globally bind KEY to COMMAND, overriding both major and minor modes."
  :debug '(form sexp)
  :ensure '(kbd func)
  :repeatable t)

(provide '+global-override-map)
;;; +global-override-map.el ends here
