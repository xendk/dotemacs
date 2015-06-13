;;; xen.el --- Assorted functions for my init.el

;; Copyright (C) 2014  Thomas Fini Hansen

;; Author: Thomas Fini Hansen <xen@claymore>
;; Keywords: local

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

;;

;;; Code:


;; My own prefix command.
(defvar xen-map)
(define-prefix-command 'xen-map)
(global-set-key (kbd "C-c x") 'xen-map)

(define-key xen-map (kbd "e")
  #'(lambda()
      "Open ~/.emacs.d/init.el."
      (interactive)
      (find-file "~/.emacs.d/init.el")))

(define-key xen-map (kbd "t")
  #'(lambda()
      "Open my Emacs TODO."
      (interactive)
      (find-file "~/.emacs.d/TODO.org")))

; Toggle fullscreen and full height.
; todo: work this in: http://bzg.fr/emacs-strip-tease.html
(defun xen-toggle-fullscreen ()
  "Toggle full screen on X11."
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (if (equal (frame-parameter nil 'fullscreen) 'fullheight) 'fullboth 'fullheight))))

(defvar xen-big-fringe-mode nil)
(define-minor-mode xen-big-fringe-mode
  "Minor mode to use big fringe in the current buffer."
  :init-value nil
  :global t
  :variable xen-big-fringe-mode
  :group 'editing-basics
  (if (not xen-big-fringe-mode)
      (fringe-mode nil)
    (fringe-mode
     (/ (- (frame-pixel-width)
           (* 120 (frame-char-width)))
        2))))

;; Version of drupal-mode-beginning-of-line that use
;; xen-back-to-indentation-or-beginning instead of beginning-of-line.
(defun xen-drupal-mode-beginning-of-line (&optional n)
  "Move point to beginning of property value or to beginning of line.
The prefix argument N is passed directly to `beginning-of-line'.

This command is identical to
`xen-back-to-indentation-or-beginning' if not in a mode derived
from `conf-mode'.

If point is on a (non-continued) property line, move point to the
beginning of the property value or the beginning of line,
whichever is closer.  If point is already at beginning of line,
move point to beginning of property value.  Therefore, repeated
calls will toggle point between beginning of property value and
beginning of line.

Heavily based on `message-beginning-of-line' from Gnus."
  (interactive "p")
  (let ((zrs 'zmacs-region-stays))
    (when (and (featurep 'xemacs) (interactive-p) (boundp zrs))
      (set zrs t)))
  (if (derived-mode-p 'conf-mode)
      (let* ((here (point))
             (bol (progn (beginning-of-line n) (point)))
             (eol (point-at-eol))
             (eoh (re-search-forward "= *" eol t)))
        (goto-char
         (if (and eoh (or (< eoh here) (= bol here)))
             eoh bol)))
    (xen-back-to-indentation-or-beginning)))


; From http://www.emacswiki.org/emacs/BackToIndentationOrBeginning
; Go back to indentation or beginning of line.
(defun xen-back-to-indentation-or-beginning ()
  "Move to beginning of indentation or line."
  (interactive)
  (if (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line)))

; Activate flyspell and yas in magit commit buffer.
(defun xen-magit-log-edit-mode-hook ()
  "Activate yas and flyspell modes."
  (yas-minor-mode 1)
  (flyspell-mode))

; I just want the branch to have the same name as origin.
(defun xen-magit-default-tracking-name
  (remote branch)
  "Use just the branch name for tracking branches.

Ignores REMOTE and just returns BRANCH."
  branch)

; Let projectile show the magit status buffer when switching to a project.
(defun xen-projectile-magit ()
  "Open magit when switching to project."
  (call-interactively 'magit-status))

(defun xen-find-file-dwim (&optional prefix)
  "Find file, in project if Projectile is active or using helm normally.

Adds file at point, if found.

Use PREFIX to disable Projectile."
  (interactive "P")
  (if (and (null prefix) (projectile-project-p))
      (let* (
             ;; Show full paths.
             (helm-ff-transformer-show-only-basename nil)
             ;; All files in project.
             (project-files (projectile-current-project-files))
             ;; Files matching thing at point.
             (files (xen-projectile-select-files project-files))
             ;; Helm sources we'll use.
             (src ()))
         (progn
           (if (= (length files) 1)
               ;; Only one hit, open it.
               (find-file (expand-file-name (car files) (projectile-project-root)))
             (progn
               (if (> (length files) 1)
                   ;; Add file candidates at point.
                   (setq src (cons (xen-helm-projectile-build-dwim-source files) src)))
               ;; Add in regular helm-projectile sources.
               (setq src (append src helm-projectile-sources-list))
               (helm :sources src
                     :buffer "*helm projectile*"
                     :prompt (projectile-prepend-project-name "pattern: "))))))
    (helm-for-files)))

(defun xen-projectile-select-files (project-files &optional arg)
  "Select a list of files based on filename at point.

The difference from `projectile-select-files' is that this doesn't return all
files when no file was found at point.

PROJECT-FILES is all files of project.

With a prefix ARG invalidates the cache first."
  (projectile-maybe-invalidate-cache arg)
  (let* ((file (if (region-active-p)
                   (buffer-substring (region-beginning) (region-end))
                 (or (thing-at-point 'filename) nil)))
         (file (if (and file (string-match "\\.?\\./" file))
                   (file-relative-name (file-truename file) (projectile-project-root))
                 file))
         (files (if file
                    (-filter (lambda (project-file)
                               (string-match file project-file))
                             project-files)
                  ())))
    files))

(defun xen-helm-projectile-build-dwim-source (candidates)
  "Dynamically build a Helm source definition for Projectile files based on context with CANDIDATES from file at point."
  ""
  (helm-build-in-buffer-source "Projectile files at point"
    :data candidates
    :fuzzy-match helm-projectile-fuzzy-match
    :coerce 'helm-projectile-coerce-file
    :action-transformer 'helm-find-files-action-transformer
    :keymap helm-projectile-find-file-map
    :help-message helm-ff-help-message
    :mode-line helm-ff-mode-line-string
    :action helm-projectile-file-actions))

; http://emacswiki.org/emacs/CopyingWholeLines
;; duplicate current line
(defun xen-duplicate-current-line (&optional n)
  "Duplicate current line, make more than 1 copy given a numeric argument.

Use prefix argument N for more copies."
  (interactive "p")
  (save-excursion
    (let ((nb (or n 1))
    	  (current-line (thing-at-point 'line)))
      ;; when on last line, insert a newline first
      (when (or (= 1 (forward-line 1)) (eq (point) (point-max)))
    	(insert "\n"))

      ;; now insert as many time as requested
      (while (> n 0)
    	(insert current-line)
    	(decf n)))))

;; Copy-paste to/from the outside.
(defvar xen-paste-buffer "" "Local paste buffer.")
(defun xen-paste ()
 "Paste from outside."
 (interactive)
 ; x-selection-value returns nil when selection hasn't changed.
 (setq xen-paste-buffer (or (x-selection-value) xen-paste-buffer))
 (insert xen-paste-buffer))

(defun xen-paste-term ()
 "Paste from outside in term-mode."
 (interactive)
 ; x-selection-value returns nil when selection hasn't changed.
 (setq xen-paste-buffer (or (x-selection-value) xen-paste-buffer))
 (term-send-raw-string xen-paste-buffer))

(defun xen-copy (start end)
  "Copy to the outside.

Copies the text from START to END."
  (interactive "r")
  (x-select-text (buffer-substring-no-properties start end)))

;; Pairing.
(defun xen-open ()
  "Open new line, with proper indentation."
  (interactive)
  (call-interactively 'move-beginning-of-line)
  (call-interactively 'open-line)
  (indent-for-tab-command))

(defun xen-paired-delete (backwards-p &optional arg)
  "Deletes the matching pair if deleting a pair.  BACKWARDS-P indicates whether we're deleting forwards or backwards and we'll only work when ARG is 1 or the region is active.."
    (when (and (= arg 1)
             smartparens-mode
             (not (use-region-p)))
        (-if-let (ok (sp-get-thing backwards-p))
            (sp-get ok (progn
                         ;; If either open or close is empty, bomb
                         ;; out. This is the case for symbols, and
                         ;; anyway it doesn't make sense.
                         (if (not (or (equal :op "") (equal :cl "")))
                             (save-excursion
                               (if (= (- (point) (if backwards-p 1 0)) :beg)
                                   (progn (goto-char :end)
                                          (message (number-to-string :end))
                                          (delete-char -1)
                                          ;; This is odd, but without
                                          ;; this, it would chomp 2
                                          ;; chars when deleting
                                          ;; forwards.
                                          (goto-char :beg)
                                          ))
                               (if (= (+ (point) (if backwards-p 0 1)) :end)
                                   (progn (goto-char :beg)
                                          (delete-char 1))))))))))



(defadvice delete-backward-char (before xen-delete-backwards-advice activate)
  (save-match-data
    (xen-paired-delete t (ad-get-arg 0))))

(defadvice delete-forward-char (before xen-delete-forward-advice activate)
  (save-match-data
    (xen-paired-delete nil (ad-get-arg 0))))


(put 'xen-paired-delete 'delete-selection 'supersede)

(defun xen-char-syntax ()
"Show the syntax class of the character following point."
(interactive)
(message (char-to-string (char-syntax (char-after)))))

(defun xen-tab ()
  "Indent if on whitespace or do nothing (auto-complete/company and yasnippet will attach themselves.)."
  (interactive "*")
  (if (or (bolp) ; Beginning of line
          (region-active-p) ; We have an active region
          (eq (char-syntax (char-before)) ?\ ) ; Or whitespace
          )
      (indent-for-tab-command)))

;; Multi-term.
(defun xen-multi-term-dedicated-toggle-and-select ()
  "Toggle dedicated `multi-term' window and select it."
  (interactive)
  (if (multi-term-dedicated-exist-p)
      (multi-term-dedicated-close)
    (progn (multi-term-dedicated-open) (multi-term-dedicated-select))))

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
            (goto-char start)
            ))
      (end-of-line)
      (forward-char))))

(defun xen-coding-common-bindings ()
  "Common bindings and minor-modes for coding."
  (local-set-key (kbd "C-o") 'xen-open)
  ;; (local-set-key [return] 'newline-and-indent)
  (local-set-key [tab] 'xen-tab)
  (local-set-key [S-iso-lefttab] 'indent-for-tab-command)
  (highlight-symbol-mode)
  (local-set-key (kbd "M-<left>") 'highlight-symbol-prev)
  (local-set-key (kbd "M-<right>") 'highlight-symbol-next)
  (local-set-key (kbd "M-<up>") 'flycheck-previous-error)
  (local-set-key (kbd "M-<down>") 'flycheck-next-error)
  (flyspell-prog-mode)
)

;; expand-region stuff.
(defun xen-php-mark-next-accessor ()
  "Presumes that current symbol is already marked, skips over one arrow and marks next symbol."
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
              (looking-back symbol-regexp))
      (skip-syntax-backward "_w.")
      (set-mark (point))
      (while (looking-at symbol-regexp)
        (forward-char))
      (if (looking-at "(\\|\\[")
          (forward-list))
      (exchange-point-and-mark))))

(defun xen-php-mode-expansions ()
  "My expand-region setup for php-mode,"
  (make-variable-buffer-local 'er/try-expand-list)
  (setq er/try-expand-list '(er/mark-word er/mark-symbol er/mark-symbol-with-prefix xen-php-mark-next-accessor xen-php-mark-method-call-or-array er/mark-comment er/mark-comment-block er/mark-inside-quotes er/mark-outside-quotes er/mark-inside-pairs er/mark-outside-pairs)))

;; Geben hackery.
(defun xen-geben-open ()
  "Open the current buffer in geben."
  (interactive)
  (progn
    (let ((geben-current-session (car geben-sessions)))
      (geben-open-file (geben-source-fileuri geben-current-session (buffer-file-name)))
      )
    )
  )

;; Gotten from http://endlessparentheses.com/easily-create-github-prs-from-magit.html
(defun xen/visit-pull-request-url ()
  "Visit the current branch's PR on Github."
  (interactive)
  (browse-url
   (format "https://github.com/%s/compare/%s"
     (replace-regexp-in-string
      "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
      (magit-get "remote"
                 (magit-get-current-remote)
                 "url"))
     (magit-get-current-branch))))

(eval-after-load 'magit
  '(define-key magit-mode-map "V"
     #'xen/visit-pull-request-url))


(provide 'xen)
;;; xen.el ends here
