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
(defvar er/try-expand-list)
(defvar flycheck-phpcs-standard)

(defgroup xen nil
  "Personal configuration"
  :group 'emacs)

;; My own prefix command. Bound in init.el.
(defvar xen-map)
(define-prefix-command 'xen-map)

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

;; Multi-term.
;; (defun xen-multi-term-dedicated-toggle-and-select ()
;;   "Toggle dedicated `multi-term' window and select it."
;;   (interactive)
;;   (if (multi-term-dedicated-exist-p)
;;       (multi-term-dedicated-close)
;;     (progn (multi-term-dedicated-open) (multi-term-dedicated-select))))

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

;; expand-region stuff.
(defun xen-php-mark-next-accessor ()
  "Presuming that current symbol is already marked, skip over one arrow and mark the next symbol."
  (interactive)
  (when (use-region-p)
    (when (< (point) (mark))
      (exchange-point-and-mark))
    (let ((symbol-regexp "\\s_\\|\\sw"))
      (when (looking-at "->")
        (forward-char 2)
        (skip-syntax-forward "_w")
        (exchange-point-and-mark)))))

(defun xen-php-mark-method-call-or-array ()
  "Mark the current symbol (including arrow) and then paren/brace to closing paren/brace."
  (interactive)
  (let ((symbol-regexp "\\s_\\|\\sw\\|->\\|>"))
    (when (or (looking-at symbol-regexp)
              (looking-back symbol-regexp 1))
      (skip-syntax-backward "_w.")
      (set-mark (point))
      (while (looking-at symbol-regexp)
        (forward-char))
      (if (looking-at "(\\|\\[")
          (forward-list))
      (exchange-point-and-mark))))

(defun xen-php-mode-expansions ()
  "My expand-region setup for php-mode."
  (make-local-variable 'er/try-expand-list)
  (setq er/try-expand-list '(er/mark-word er/mark-symbol er/mark-symbol-with-prefix xen-php-mark-next-accessor xen-php-mark-method-call-or-array er/mark-comment er/mark-comment-block er/mark-inside-quotes er/mark-outside-quotes er/mark-inside-pairs er/mark-outside-pairs)))

;; Geben hackery.
;; (defun xen-geben-open ()
;;   "Open the current buffer in geben."
;;   (interactive)
;;   (progn
;;     (let ((geben-current-session (car geben-sessions)))
;;       (geben-open-file (geben-source-fileuri geben-current-session (buffer-file-name))))))

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
  "When in minibuffer or term-char-mode disable `emulation-mode-map-alists'.

Else just call `avy-goto-word-1'"
  (interactive)
  (if (or (window-minibuffer-p)
          (and (eq major-mode 'term-mode)
               (when (fboundp 'term-in-char-mode) (term-in-char-mode))))
      (let ((emulation-mode-map-alists nil)
            (binding (key-binding (kbd "S-<SPC>") t)))
        (when binding
          (call-interactively binding)))
    (call-interactively 'avy-goto-word-1)))

;; Define a FontAwesome face.
(make-face 'xen-font-awesome-face)
(set-face-attribute 'xen-font-awesome-face nil
                    :family "FontAwesome")


(defun xen-changelog-timestamp (&optional time zone)
  "Return a time string for ~/ChangeLog.

Passes TIME and ZONE to `format-time-string.'"
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S" time zone)))

(defun xen-dashboard-tip (list-size)
  "Insert a tip into the dashboard.

LIST-SIZE is ignored."
  (let ((tips (with-temp-buffer
                (insert-file-contents (locate-user-emacs-file "tips"))
                (split-string (buffer-string) "\f" t))))
    (insert (elt tips (random (length tips))))))

(defun xen-dashboard-todo (list-size)
  "Insert todo in the dashboard.

LIST-SIZE is ignored."
  (insert-file-contents (locate-user-emacs-file "todo")))

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

(defun xen-php-spec ()
  "Set PHPCS to use PHPSpec standard."
  (interactive)
  (make-local-variable 'flycheck-phpcs-standard)
  (setq flycheck-phpcs-standard "/home/xen/.config/composer/vendor/kmcculloch/phpspec-code-sniffer/PHPSpec/"))

(defun xen-cycle-spacing (&optional n)
  "Delete all spaces and tabs around point, leaving one space (or N spaces).
If N is negative, delete newlines as well, leaving -N spaces.

Subsequent calls will delete all spaces, or revert to the original spacing.

See also `cycle-spacing'."
    (interactive "*p")
    (cycle-spacing n nil 'fast))

(defun xen-git-commit-setup ()
  "Insert Jira issue number in commit message if branch name contain one."
  (let ((ISSUEKEY "[[:alpha:][:digit:]]+-[[:digit:]]+"))
    (when (and (magit-get-current-branch)
               (string-match-p ISSUEKEY (magit-get-current-branch))
               (looking-at-p "\n\n#"))
      (insert
       (concat
        "\n\nSee "
        (upcase (replace-regexp-in-string
                 (concat ".*?\\(" ISSUEKEY "\\).*")
                 "\\1"
                 (magit-get-current-branch)))))
      (push-mark)
      (goto-char (point-min)))))

(provide 'xen)
;;; xen.el ends here
