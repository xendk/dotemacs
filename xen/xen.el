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

(defun xen-toggle-font-size ()
  "Toggle font size between my usual two."
  (interactive)
  (if (= (face-attribute 'default :height) 113)
      (set-face-attribute 'default nil :height 140)
    (set-face-attribute 'default nil :height 113)))

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

;; Emacs 24 changed the region highlight from a hackery face thingy to
;; a proper overlay. Which is fine apart from giving it a nil priority
;; which puts it below pretty much everything else. So we redefine the
;; redisplay-highlight-region-function to give the overlay a higher
;; priority.
;;
;; Further inspiration:
;; https://www.reddit.com/r/emacs/comments/345by9/having_the_background_face_for_selection_region/
(eval-after-load 'simple
'(setq redisplay-highlight-region-function
  (lambda (start end window rol)
    (if (not (overlayp rol))
        (let ((nrol (make-overlay start end)))
          (funcall redisplay-unhighlight-region-function rol)
          (overlay-put nrol 'window window)
          (overlay-put nrol 'face 'region)
          ;; Flycheck uses priorities of 100-ish, so we go higher than that.
          (overlay-put nrol 'priority '(200 . 100))
          nrol)
      (unless (and (eq (overlay-buffer rol) (current-buffer))
                   (eq (overlay-start rol) start)
                   (eq (overlay-end rol) end))
        (move-overlay rol start end (current-buffer)))
      rol))))


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

; I just want the branch to have the same name as origin.
(defun xen-magit-default-tracking-name
  (remote branch)
  "Use just the branch name for tracking branches.

Ignores REMOTE and just returns BRANCH."
  branch)

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
 ;; todo: delete region if active.
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
  "Deletes the matching pair if deleting a pair.

BACKWARDS-P indicates whether we're deleting forwards or backwards and we'll
only work when ARG is 1 or the region is not active."
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

(defun xen-delete-char-advice (n &optional kill-flag)
  "Advice for delete char.  Use N and ignore KILL-FLAG."
  (if (not (boundp 'xen-delete-char-disabled))
      (let ((xen-delete-char-disabled t))
        (save-match-data (progn
                           (xen-paired-delete (> 0 n) (abs n)))))))
(advice-add 'delete-char :before #'xen-delete-char-advice)

(defun xen-sp-insert-pair-advice (orig-fun &rest args)
  "Advice to disable paired delete in sp-insert-pair/sp-skip-closing-pair.  Call ORIG-FUN with ARGS."
  (let ((xen-delete-char-disabled t))
    (apply orig-fun args)))
(advice-add 'sp-insert-pair :around #'xen-sp-insert-pair-advice)
(advice-add 'sp-skip-closing-pair :around #'xen-sp-insert-pair-advice)

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

(defun xen-xml-pretty ()
  "Run xmllint -pretty - on the region."
  (interactive)
  (if (region-active-p) (shell-command-on-region
                         (region-beginning) (region-end)
                         "xmllint -format -" nil t)))

(defun xen-avy-goto-line (&optional arg)
  "A variant on avy-goto-line that remembers column position.  ARG is passed along."
  (interactive "p")
  (let ((col-pos (current-column)))
    (progn
      (call-interactively 'avy-goto-line arg)
      (move-to-column col-pos))))

;(require 'swiper)
(defun xen-swiper ()
  "Call swiper with region (from BEG to END) as initial-input."
  (interactive)
  (swiper (if (use-region-p)
                   (progn
                     (deactivate-mark)
                     (buffer-substring-no-properties
                      (region-beginning)
                      (region-end))))))

(defun xen-psr ()
  "Switch to PSR mode."
  (interactive)
  (setq flycheck-phpcs-standard "PSR2")
  (c-set-style "psr2")
  (setq c-basic-offset 4)
  (kill-local-variable 'flycheck-disabled-checkers))

;; Define a FontAwesome face.
(make-face 'xen-font-awesome-face)
(set-face-attribute 'xen-font-awesome-face nil
                    :family "FontAwesome")

(defun xen-flycheck-mode-line-status-text (&optional status)
  "Get a text describing STATUS for use in the mode line.

STATUS defaults to `flycheck-last-status-change' if omitted or
nil.

This is my own version using FontAwesome icons."
  (let ((text (pcase (or status flycheck-last-status-change)
                (`not-checked "")
                (`no-checker (propertize (concat [#xF141])
                                         'face 'xen-font-awesome-face))
                (`running (propertize (concat [#xF110])
                                      'face 'xen-font-awesome-face))
                (`errored (propertize (concat [#xF12A])
                                      'face 'xen-font-awesome-face))
                (`finished
                 (let-alist (flycheck-count-errors flycheck-current-errors)
                   (if (or .error .warning)
                       (list (propertize (concat [#xF00D])
                                           'face 'xen-font-awesome-face)
                                (format "%s/%s" (or .error 0) (or .warning 0))
                               )
                     (propertize (concat [#xF00C])
                                 'face 'xen-font-awesome-face))))
                (`interrupted (propertize (concat [#xF127])
                                          'face 'xen-font-awesome-face))
                (`suspicious (propertize (concat [#xF128])
                                         'face 'xen-font-awesome-face)))))
    (list " " text)))

(defun xen-changelog-timestamp ()
  "Return a time string for ~/ChangeLog."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))


(provide 'xen)
;;; xen.el ends here
