;;; +avy.el --- avy additions                        -*- lexical-binding: t; -*-
;;;
;;; Commentary:

;;; Code:

(defun +avy-goto-line (&optional arg)
  "A variant on avy-goto-line that remembers column position.  ARG is passed along."
  (interactive "p")
  (let ((col-pos (current-column)))
    (progn
      (call-interactively 'avy-goto-line arg)
      (move-to-column col-pos))))

(defun +avy-goto-char-timer ()
  "Call `avy-goto-char-timer' but in a few instances.

When in minibuffer, `vterm-mode', or `completion-in-region-mode'
disable `emulation-mode-map-alists' and call the original binding.

Else just call `avy-goto-char-timer'.

This is needed to disable avy in minibuffer and terminal.
+global-override-map uses emulation-mode-map in order to override
all major and minor mode bindings. Which makes it tricky to
override in the one case where we want to.

An alternative might be a globalized minor mode map, and ensuring
the minor mode is loaded first."
  (interactive)
  (if (or (window-minibuffer-p)
          (and (eq major-mode 'vterm-mode)
               (not (bound-and-true-p vterm-copy-mode)))
          ;; Corfu has it's own quick keys.
          completion-in-region-mode)
      (let ((emulation-mode-map-alists nil)
            (binding (key-binding (kbd "S-<SPC>") t)))
        (when binding
          (call-interactively binding)))
    (call-interactively 'avy-goto-char-timer)))

(provide '+avy)
;;; +avy.el ends here
