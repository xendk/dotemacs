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
      "Open tips file."
      (interactive)
      (find-file "~/.emacs.d/tips")))

(define-key xen-map (kbd "o")
  #'(lambda()
      "Open todo file."
      (interactive)
      (find-file "~/.emacs.d/todo")))

(defun xen-toggle-font-size ()
  "Toggle font size between my usual two."
  (interactive)
  (if (= (face-attribute 'default :height) 110)
      (set-face-attribute 'default nil :height 140)
    (set-face-attribute 'default nil :height 110)))

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

;; TODO: auto-indent-mode messes with this.
(defun xen-open ()
  "Open new line, with proper indentation."
  (interactive)
  (beginning-of-line)
  (call-interactively 'open-line)
  (indent-for-tab-command))

;; Pairing.
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
                     ;; anyway it doesn't make sense. Look into
                     ;; sp-navigate-consider-symbols for
                     ;; sp-get-thing.
                     (unless (or (equal :op "") (equal :cl ""))
                       (cond
                        ((and backwards-p (or (= (point) :beg-in) (= (point) :end)))
                         (sp-backward-unwrap-sexp))
                        ((and (not backwards-p) (or (= (point) :beg) (= (point) :end-in)))
                         (sp-unwrap-sexp))
                        (t nil))))))))

(defun xen-delete-char-advice (orig-fun n &optional kill-flag)
  "Advice for delete char.  ORIG-FUN is the overriden function. Use N and ignore KILL-FLAG."
  (if (not (boundp 'xen-delete-char-disabled))
      (let ((xen-delete-char-disabled t))
        (save-match-data (progn
                           (if (not (xen-paired-delete (> 0 n) (abs n)))
                               (funcall orig-fun n kill-flag)))))
    (funcall orig-fun n kill-flag)))
(advice-add 'delete-char :around #'xen-delete-char-advice)

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
  (flyspell-prog-mode)
)

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
              (looking-back symbol-regexp))
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
(defun xen-geben-open ()
  "Open the current buffer in geben."
  (interactive)
  (progn
    (let ((geben-current-session (car geben-sessions)))
      (geben-open-file (geben-source-fileuri geben-current-session (buffer-file-name)))
      )
    )
  )

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

(defun xen-changelog-timestamp (&optional time zone)
  "Return a time string for ~/ChangeLog."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S" time zone)))

(defun xen-company-complete-common-or-selection ()
  "Insert the common part of all candidates, or select the current one."
  (interactive)
  (when (company-manual-begin)
    (let ((tick (buffer-chars-modified-tick)))
      (call-interactively 'company-complete-common)
      (when (eq tick (buffer-chars-modified-tick))
        (company-complete-selection)))))

;; Gotten from:
;; http://stackoverflow.com/questions/24725778/how-to-rebuild-elpa-packages-after-upgrade-of-emacs
;; Seems to think everything fails though.
(defun xen-package-reinstall-activated ()
  "Reinstall all activated packages."
  (interactive)
  (dolist (package-name package-activated-list)
    (when (package-installed-p package-name)
      (unless (ignore-errors                   ;some packages may fail to install
                (package-reinstall package-name)
                (warn "Package %s failed to reinstall" package-name))))))

(defun xen-swiper-from-isearch ()
  "Invoke `swiper' from isearch."
  (interactive)
  (let ((query (if isearch-regexp
                    isearch-string
                  (regexp-quote isearch-string))))
    (isearch-exit)
    (message query)
    (swiper query)))

(defun xen-dashboard-tip (list-size)
  "Insert a tip into the dashboard.

LIST-SIZE is ignored."
  (let ((tips (with-temp-buffer
                (insert-file-contents "~/.emacs.d/tips")
                (split-string (buffer-string) "\f" t))))
    (insert (elt tips (random (length tips))))))

(defun xen-dashboard-todo (list-size)
  "Insert todo in the dashboard.

LIST-SIZE is ignored."
  (insert-file-contents "~/.emacs.d/todo"))

(provide 'xen)
;;; xen.el ends here
