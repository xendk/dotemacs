;;; +jump-to-matching.el --- jump to matching -*- lexical-binding: t; -*-

;;; Commentary:

;; Inspired by
;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Matching-parentheses.html

;;; Code:

(defvar sp-show-pair-from-inside)

(defun +jump-to-matching ()
  "Go to the matching parenthesis/bracket/brace if on a parenthesis/bracket/brace.

When `sp-show-pair-from-inside' is t, it also works when point is
after the parenthesis/bracket/brace, to mirror show-smartparens-mode
behavior. This can cause asymmetry in jumping back and forth as the
character after point is prioritized."
  (interactive)
  (let ((lookbehind-too (bound-and-true-p sp-show-pair-from-inside)))
    (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
          ((looking-at "\\s)") (forward-char 1) (backward-list 1))
          ;; Also look before point.
          ((and lookbehind-too (looking-back "\\s(" 1)) (backward-char 1) (forward-list 1))
          ((and lookbehind-too (looking-back "\\s)" 1)) (backward-list 1) (forward-char 1))
          (t (user-error "Not on paired char")))))

(provide '+jump-to-matching)
;;; +jump-to-matching.el ends here
