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
  "Use just the branch name for tracking branches."
  branch)

; Let projectile show the magit status buffer when switching to a project.
(defun xen-projectile-magit ()
  "Open magit when switching to project."
  (call-interactively 'magit-status))

(defun xen-find-file (&optional prefix)
  "Find file, in project if Projectile is active or using helm normally"
  (interactive "P")
  (if (and (null prefix) (projectile-project-p))
      (helm-projectile)
    (helm-for-files)))

; http://emacswiki.org/emacs/CopyingWholeLines
;; duplicate current line
(defun xen-duplicate-current-line (&optional n)
  "duplicate current line, make more than 1 copy given a numeric argument"
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
 (insert xen-paste-buffer)
)

(defun xen-paste-term ()
 "Paste from outside in term-mode."
 (interactive)
 ; x-selection-value returns nil when selection hasn't changed.
 (setq xen-paste-buffer (or (x-selection-value) xen-paste-buffer))
 (term-send-raw-string xen-paste-buffer)
)

(defun xen-copy (start end)
  "Copy to the outside.

Copies the text from START to END."
  (interactive "r")
  (x-select-text (buffer-substring-no-properties start end))
)


(provide 'xen)
;;; xen.el ends here
