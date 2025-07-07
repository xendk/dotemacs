;;; +magit.el --- magit additions                    -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(declare-function magit-get-current-branch "magit")

(defun +magit-commit-setup-jira ()
  "Insert Jira issue number in commit message if branch name contain one."
  (let ((ISSUEKEY "[[:alpha:][:digit:]]\\{2,\\}-[[:digit:]]+"))
    (when (and (magit-get-current-branch)
               (string-match-p ISSUEKEY (magit-get-current-branch))
               (looking-at-p "\n\n#"))
      (insert
       (concat
        "\n\nRef "
        (upcase (replace-regexp-in-string
                 (concat ".*?\\(" ISSUEKEY "\\).*")
                 "\\1"
                 (magit-get-current-branch)))))
      (push-mark)
      (goto-char (point-min)))))

;; If there's an untouched CHANGELOG.md in the repo, ask for
;; confirmation. Could check if any changes to the file is actually
;; staged, but this covers the most common case of "forgot to update
;; the changelog".
;; TODO: y/n/e to quickly edit it (recursive edit?).
(define-advice magit-commit-create
    (:before-until (orig-fun &rest args) +magit-commit-changelog-check)
  (if (file-exists-p "CHANGELOG.md")
      (unless (magit-file-status "CHANGELOG.md")
        (not (y-or-n-p "Changelog not updated, continue?")))))

(provide '+magit)
;;; +magit.el ends here
