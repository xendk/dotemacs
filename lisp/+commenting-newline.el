;;; +commenting-newline.el --- commenting newline -*- lexical-binding: t; -*-

;;; Commentary:

;; Originally this should make newline in single line comments work
;; much like online WYSIWYG editors does blocks. I.e. a newline adds a
;; new empty comment line, newline on the first empty comment inserts
;; another empty comment, but the third empty line "breaks out" of
;; commenting, deleting the last empty comments.

;; But obviously it's not quite there as it deletes after the first
;; empty comment. Needs a bit more work.

;;; Code:

(defun +commenting-newline (&optional arg interactive)
  "Insert a newline, handling comments.

Uses `default-indent-new-line' in comments and `newline' otherwise.

After two empty line comments, it'll delete both.

Pass ARG and INTERACTIVE to `newline'."
  (interactive "*P\np")
  (barf-if-buffer-read-only)

  ;; Use syntax table to determine if we're in a comment (gleaned from
  ;; mwim).
  (if (+commenting-newline-in-comment)
      ;; Remove the comment if we're looking at two empty single line
      ;; comments.
      (let ((empty-comment-start (+commenting-newline-empty-comment-start)))
        (if empty-comment-start
            (delete-region empty-comment-start (point))
          (+commenting-newline-default-indent-new-line)))
    (newline arg interactive)))

(defun +commenting-newline-default-indent-new-line ()
  "Call `default-indent-new-line', or handles `emacs-lisp-mode' specifically."
  (if (eq major-mode 'emacs-lisp-mode)
      (let (comment-start num-semis)
        (save-excursion
          (goto-char (+commenting-newline-in-comment))
          (setq comment-start (point))
          (skip-chars-forward ";")
          (setq num-semis (- (point) comment-start)))
        (insert "\n")
        (insert (make-string num-semis ?\;) " "))
    (default-indent-new-line)))

(defun +commenting-newline-comment-line-is-empty ()
  "Determine if comment is empty."
  (let ((in-comment (+commenting-newline-in-comment)))
    (if in-comment
        (save-excursion
          (goto-char in-comment)
          ;; In modes without a unique comment character (php-mode being one
          ;; example), the start comment char(s) doesn't have the comment
          ;; start syntax. So look up the syntax of the first char in the
          ;; comment and skip that class.
          (let ((syntax-class (char-syntax (char-after))))
            (skip-syntax-forward (string syntax-class))
            ;; Skip white-space.
            (skip-syntax-forward "-")
            ;; > is the "comment ender" syntax, which is applied to a
            ;; comment-ending newline.
            (or (eobp) (eq (char-syntax (char-after)) ?>)))))))

(defun +commenting-newline-in-comment ()
  "Return start of comment, or nil if not inside comment."
  (let ((syn (syntax-ppss)))
    ;; These are documented as:
    ;;  > 4. nil if outside a comment, t if inside a non-nestable comment,
    ;;  > else an integer (the current comment nesting).
    ;;  > 8. character address of start of comment or string; nil if not in one.
    ;; So if 4 is t, we're in a comment and can return 8.
    (and (nth 4 syn)
         (nth 8 syn))))

(defun +commenting-newline-empty-comment-start ()
  "Return start of empty comment block.

Returns the start of the first empty comment if we're on the second
empty single line comment."
  (when comment-start
    (ignore-errors
      (if (+commenting-newline-comment-line-is-empty)
          (save-excursion
            (let ((this-start (+commenting-newline-in-comment))
                  ;; Trimming comment-start as modes often include a
                  ;; space which is technically just part of the comment.
                  (trimmed-comment-start (string-trim-right comment-start)))
              (forward-line -1)
              (end-of-line)
              (if (+commenting-newline-in-comment)
                  (when (+commenting-newline-comment-line-is-empty)
                    (let ((start-of-comment (+commenting-newline-in-comment)))
                      ;; We got an empty comment on the previous
                      ;; line, return it, if it matches the
                      ;; comment-start (block comments shouldn't).
                      (progn
                        (goto-char start-of-comment)
                        (if (looking-at-p trimmed-comment-start)
                            start-of-comment))))
                ;; Return the start of the original if there's no
                ;; comment on the previous line.
                (save-excursion
                  (goto-char this-start)
                  (if (looking-at-p trimmed-comment-start)
                      this-start)))))))))

(provide '+commenting-newline)
;;; +commenting-newline.el ends here
