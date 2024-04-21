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

(provide '+avy)
;;; +avy.el ends here
