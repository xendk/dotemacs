;;; xen.el --- Assorted functions for my init.el

;; Copyright (C) 2014  Thomas Fini Hansen

;; Author: Thomas Fini Hansen <xen@claymore>
;; Keywords: local
;; Package-Requires: ((emacs "25"))
;; Package-Version: 0
;; Url: https://github.com/xendk/dotemacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is bits and pieces of configuration and handy functions for my
;; setup. Moved here to unclutter init.el.

;;; Code:

;; External variables referenced.
(defvar vterm-copy-mode)

(declare-function magit-get-current-branch "magit-git.el")

(defgroup xen nil
  "Personal configuration"
  :group 'emacs)

;; My own prefix command. Bound in init.el.
(defvar xen-map)
(define-prefix-command 'xen-map)

(defvar xen-casing-map)
(define-prefix-command 'xen-casing-map)

(define-key xen-map (kbd "e")
  #'(lambda()
      "Open init.el."
      (interactive)
      (find-file (locate-user-emacs-file "init.el"))))

(define-key xen-map (kbd "t")
  #'(lambda()
      "Open tips file."
      (interactive)
      (find-file (locate-user-emacs-file "tips"))))

(define-key xen-map (kbd "o")
  #'(lambda()
      "Open todo file."
      (interactive)
      (find-file (locate-user-emacs-file "todo"))))

(defvar xen-big-fringe-mode nil)
(define-minor-mode xen-big-fringe-mode
  "Minor mode to use big fringe in the current buffer."
  :init-value nil
  :global t
  ;; emacs-lisp-package require this, but I don't think it'll work.
  :require 'xen
  :variable xen-big-fringe-mode
  :group 'editing-basics
  (if (not xen-big-fringe-mode)
      (fringe-mode nil)
    (fringe-mode
     (/ (- (frame-pixel-width)
           (* 120 (frame-char-width)))
        2))))

;; Originally duplicate-current-line from
;; http://emacswiki.org/emacs/CopyingWholeLines, but that have
;; problems at the end of buffer. This version adapted from:
;; https://stackoverflow.com/questions/88399/how-do-i-duplicate-a-whole-line-in-emacs
(defun xen-duplicate-current-line (&optional arg)
  "Duplicate current line, make more than 1 copy given a numeric argument.

Use prefix argument ARG for more copies."
  (interactive "*p")
  ;; Save the point for undo.
  (setq buffer-undo-list (cons (point) buffer-undo-list))

  ;; Local variables for start and end of line.
  (save-excursion
    (let ((bol (progn (beginning-of-line) (point)))
          ;; Don't use forward-line for this, because you would have
          ;; to check whether you are at the end of the buffer.
          (eol (progn (end-of-line) (point))))
      ;; Store the line and disable the recording of undo information.
      (let ((line (buffer-substring bol eol))
            ;; Undo history is not recorded while it's t.
            (buffer-undo-list t)
            (count arg))
        ;; Insert the line arg times.
        (while (> count 0)
          (newline)         ;; Because there is no newline in 'line'.
          (insert line)
          (setq count (1- count))))

      ;; Mark the inserted region as one insert and add it to the undo list.
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))))

(defun xen-open ()
  "Open new line, with proper indentation."
  (interactive)
  (beginning-of-line)
  (call-interactively 'open-line)
  (indent-for-tab-command))

(defun xen-char-syntax ()
  "Show the syntax class of the character following point."
  (interactive)
  (message (char-to-string (char-syntax (char-after)))))

(defun xen-tab ()
  "Indent if on whitespace or do nothing (auto-complete/company and yasnippet will attach themselves.)."
  (interactive "*")
  (if (or (bolp) ; Beginning of line
          (region-active-p) ; We have an active region
          (eq (char-syntax (char-before)) ?\ )) ; Or whitespace
      (indent-for-tab-command)))

(defun xen-mark-lines ()
  "Mark the current line, or expand the selection to another line.

Actually shrinks the region if the point is at the start of the region."
  (interactive)
  (let ((start (point)))
    (progn
      (if (not (region-active-p))
          (progn
            (beginning-of-line)
            (set-mark (point))
            (goto-char start)))
      (end-of-line)
      (forward-char))))

(defun xen-coding-common-bindings ()
  "Common bindings and minor-modes for coding."
  (local-set-key (kbd "C-o") 'xen-open)
  (local-set-key [tab] 'xen-tab)
  (local-set-key [backtab] 'indent-for-tab-command))

(defun xen-xml-pretty ()
  "Run xmllint -pretty - on the region."
  (interactive)
  (if (region-active-p) (shell-command-on-region
                         (region-beginning) (region-end)
                         "xmllint -format -" nil t)))

(defun xen-json-pretty ()
  "Run jq . on the region."
  (interactive)
  (if (region-active-p) (shell-command-on-region
                         (region-beginning) (region-end)
                         "jq ." nil t)))

(defun xen-json-unpretty ()
  "Run jq -c . on the region."
  (interactive)
  (if (region-active-p) (shell-command-on-region
                         (region-beginning) (region-end)
                         "jq -c ." nil t)))

(defun xen-avy-goto-line (&optional arg)
  "A variant on avy-goto-line that remembers column position.  ARG is passed along."
  (interactive "p")
  (let ((col-pos (current-column)))
    (progn
      (call-interactively 'avy-goto-line arg)
      (move-to-column col-pos))))

(defun xen-avy-goto-word-1 ()
  "When in minibuffer or vterm-mode disable `emulation-mode-map-alists'.

Else just call `avy-goto-word-1'.

This hackery is needed to disable avy in minibuffer and terminal.
The :bind* stanza for use-package makes it use an
emulation-mode-map in order to override all major and minor mode
bindings. Which makes it tricky to override in the one case where
we want to.

An alternative might be a globalized minor mode map, and ensuring
the minor mode is loaded first."
  (interactive)
  (if (or (window-minibuffer-p)
          (and (eq major-mode 'vterm-mode)
               (when (fboundp 'vterm-copy-mode) (not vterm-copy-mode))))
      (let ((emulation-mode-map-alists nil)
            (binding (key-binding (kbd "S-<SPC>") t)))
        (when binding
          (call-interactively binding)))
    (call-interactively 'avy-goto-word-1)))

;; Define a FontAwesome face.
(make-face 'xen-font-awesome-face)
(set-face-attribute 'xen-font-awesome-face nil
                    :family "FontAwesome")

(defun xen-dashboard-tip (list-size)
  "Insert a tip into the dashboard.

LIST-SIZE is ignored."
  (dashboard-insert-heading "Tip of the day" "t")
  (insert "\n")
  (let ((tips (with-temp-buffer
                (insert-file-contents (locate-user-emacs-file "tips"))
                (split-string (buffer-string) "\f" t))))
    (insert (elt tips (random (length tips)))))
  (dashboard-insert-shortcut "t" "Tip of the day"))

(defun xen-dashboard-todo (list-size)
  "Insert todo in the dashboard.

LIST-SIZE is ignored."
  (dashboard-insert-heading "Todo" "o")
  (insert "\n")
  (insert-file-contents (locate-user-emacs-file "todo"))
  (dashboard-insert-shortcut "o" "Todo"))

;; misc minor modes

(defun xen-fix-minor-mode-order (file)
  "For `after-load-functions'. Prioritizes some minor-modes.
FILE is ignored."
  (when (assq 'drupal-mode minor-mode-map-alist)
    (when (not (eq (car (car minor-mode-map-alist)) 'drupal-mode))
      (let ((mode (assq 'drupal-mode minor-mode-map-alist)))
        (assq-delete-all 'drupal-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist mode)))))
(add-hook 'after-load-functions 'xen-fix-minor-mode-order)

(defun xen-cycle-spacing (&optional n)
  "Delete all spaces and tabs around point, leaving one space (or N spaces).
If N is negative, delete newlines as well, leaving -N spaces.

Subsequent calls will delete all spaces, or revert to the original spacing.

See also `cycle-spacing'."
    (interactive "*p")
    (cycle-spacing n nil 'fast))

(defun xen-git-commit-setup ()
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

(defun xen-region-isearch-forward ()
  "Start an isearh with the content of the region."
  (interactive)
  (let ((string (buffer-substring-no-properties
                 (region-beginning) (region-end))))

    (deactivate-mark)
    ;; Move point so that the isearch wont start by finding the instance
    ;; we just deselected.
    (goto-char (region-end))
    (isearch-forward nil 1)
    (isearch-yank-string string)))

(defun xen-region-isearch-backward ()
  "Start an isearh-backward with the content of the region."
  (interactive)
  (let ((string (buffer-substring-no-properties
                 (region-beginning) (region-end))))
    (deactivate-mark)
    ;; Move point so that the isearch wont start by finding the instance
    ;; we just deselected.
    (goto-char (region-beginning))
    ;; And when searching backwards we need to move a char further
    ;; back. But that would cause an error at the beginning of buffer,
    ;; so handle that. This means that if the region was at the
    ;; beginning of the buffer, we'll still find the marked instance,
    ;; but it's a bug we can live with.
    (when (> (point) 1)
      (forward-char -1))
    (isearch-backward nil 1)
    (isearch-yank-string string)))

(provide 'xen)
;;; xen.el ends here
