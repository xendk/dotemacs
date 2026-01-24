;;; +magit.el --- magit additions                    -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(declare-function magit-commit-create "magit")
(declare-function magit-get-current-branch "magit")
(declare-function magit-file-status "magit")
(declare-function magit-stage-files "magit")
(declare-function keepachangelog-add-entry "keepachangelog")
(declare-function nerd-icons-devicon "nerd-icons")

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
               (answer (read-answer "Update changelog? "
                                    '(("yes" ?y "abort commit and add to changelog")
                                      ("no" ?n "continue committing")
                                      ("abort" ?a "abort commit")))))
          (cond ((equal answer "no") nil)
                ;; This could enable a minor-mode which binds C-c C-c
                ;; to a function that stages the CHANGELOG.md file and
                ;; calls `magit-commit-create' again.
                ((equal answer "yes")
                 (call-interactively #'keepachangelog-add-entry)
                 (+magit-changelog-commit-mode 1)
                 t)
                (t t))))))

(defvar +magit-changelog-commit-mode-lighter
  '(:eval (when +magit-changelog-commit-mode
            (concat " " (nerd-icons-devicon "nf-dev-git"))))
  "Mode lighter for +magit-changelog-commit-mode.")

(put '+magit-changelog-commit-mode-lighter 'risky-local-variable t)

(defvar +magit-changelog-commit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'+magit-commit-changelog)
    map)
  "Keymap for `+magit-changelog-commit-mode'.")

(define-minor-mode +magit-changelog-commit-mode
  "Minor mode for committing after editing CHANGELOG."
  :lighter +magit-changelog-commit-mode-lighter
  :init-value nil
  :keymap +magit-changelog-commit-mode-map)

(defun +magit-commit-changelog ()
  "Save buffer, stage CHANGELOG.md, and retry commit."
  (interactive)
  (save-buffer)
  (magit-stage-files '("CHANGELOG.md"))
  (call-interactively #'magit-commit-create))

(provide '+magit)
;;; +magit.el ends here
