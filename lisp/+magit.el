;;; +magit.el --- magit additions                    -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(declare-function magit-get-current-branch "magit")
(declare-function magit-file-status "magit")
(declare-function keepachangelog-add-entry "keepachangelog")

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

;; If there's a CHANGELOG.md in the repo, and no changes is staged for
;; it, ask for confirmation.
(define-advice magit-commit-create
    (:before-until (_orig-fun &rest _args) +magit-commit-changelog-check)
  (if (file-exists-p "CHANGELOG.md")
      ;; `magit-file-status' returns (("CHANGELOG.md" nil 32 77)) for
      ;; an unstaged file, (("CHANGELOG.md" nil 77 32)) for a staged
      ;; and (("CHANGELOG.md" nil 77 77)) for a partially staged.
      (unless (eq ?M (nth 2 (car (magit-file-status "CHANGELOG.md"))))
        (let* ((read-answer-short 1)
               (answer (read-answer "Changelog not updated, continue? "
                                    '(("yes" ?y "commit")
                                      ("no" ?n "cancel")
                                      ("add" ?a "add to changelog")))))
          (cond ((equal answer "no") t)
                ;; This could enable a minor-mode which binds C-c C-c
                ;; to a function that stages the CHANGELOG.md file and
                ;; calls `magit-commit-create' again.
                ((equal answer "add") (call-interactively #'keepachangelog-add-entry) t)
                (t nil))))))

(provide '+magit)
;;; +magit.el ends here
