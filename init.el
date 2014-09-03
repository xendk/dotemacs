;;; package --- Xens emacs configuration.
;;; Commentary:
; Quick debugging:
; (toggle-debug-on-error)

;; Handy trick:
;; (set-face-attribute 'default nil :height 140)

;; Handy variables for line (non-)wrapping:
;; trucante-lines
;; word-wrap
;; Also, look into adaptive-wrap (mentioned here: http://emacswiki.org/emacs/LineWrap )

;; Highlight too long lines, clashes a bit with hl-line-mode:
;; (require 'whitespace)
;; (setq whitespace-style '(face empty tabs lines-tail trailing))
;; (global-whitespace-mode t)
;; Will also require something like the following:
;; (defadvice popup-draw (before my-turn-off-whitespace activate compile)
;;   "Turn off whitespace mode before showing autocomplete box"
;;   (if whitespace-mode
;;       (progn
;;         (setq my-prev-whitespace-mode t)
;;         (prelude-turn-off-whitespace))
;;     (setq my-prev-whitespace-mode nil)))

;; (defadvice popup-delete (after my-restore-whitespace activate compile)
;;   "Restore previous whitespace mode when deleting autocomplete box"
;;   (if my-prev-whitespace-mode
;;       (prelude-turn-on-whitespace)))

;; Try https://github.com/syohex/emacs-git-gutter for laughs..

;; Take a look at http://www.emacswiki.org/emacs/MarkCommands
;; Maybe ressurrect https://github.com/xendk/dotemacs/commit/4d718daf386ae329e9d65ec90780f0fdc55f138e

;;; Code:
(require 'server)
(if (not (server-running-p))
    (progn
      (server-start)
      (add-hook 'kill-emacs-hook (lambda () (server-start 1)))
      )
  )
; I'm grown up, I can manage using y/n for even destructive commands.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Beginning of the el4r block:
;; RCtool generated this block automatically. DO NOT MODIFY this block!
;(add-to-list 'load-path "/usr/share/emacs/site-lisp")
;(require 'el4r)
;(el4r-boot)
;; End of the el4r block.
;; User-setting area is below this line.

;; Lets get rid of the menu bar.
(menu-bar-mode -1)

(transient-mark-mode t)
(show-paren-mode)
(setq mark-even-if-inactive t)
(setq bookmark-save-flag 1)
(setq font-lock-maximum-decoration t)

(define-prefix-command 'xen-map)
(global-set-key (kbd "C-c x") 'xen-map)


(define-key xen-map (kbd "e")
  #'(lambda()
      "Open ~/.emacs.d/init.el."
      (interactive)
      (find-file "~/.emacs.d/init.el")
      )
  )
(define-key xen-map (kbd "t")
  #'(lambda()
      "Open my Emacs TODO."
      (interactive)
      (find-file "~/.emacs.d/TODO.org")
      )
)

(setq load-path (cons "~/.emacs.d/lib/" load-path))

;; Getting tired of entering passwords for sudo..
(setq password-cache-expiry 3600)

;; Setup Android SDK.
;(setq android-mode-sdk-dir "~/lib/android-sdk-linux/")
;; And load it's .el file.
;(load (concat android-mode-sdk-dir "tools/lib/android.el"))

(require 'package)
(add-to-list 'package-archives
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

; Yeah, global undo tree mode...
(global-undo-tree-mode)

; Only autopair ' when not directly after a word.
(eval-after-load "smartparens" '(sp-pair "'" nil :unless '(sp-point-after-word-p)))
; When pressing return as the first thing after inserting a { or (,
; add another and indent.
(eval-after-load "smartparens" '(sp-local-pair 'php-mode "{" nil :post-handlers '(("||\n[i]" "<return>"))))
(eval-after-load "smartparens" '(sp-local-pair 'php-mode "(" nil :post-handlers '(("||\n[i]" "<return>"))))

(eval-after-load "smartparens" '(sp-local-pair 'css-mode "/*" "*/" :actions '(wrap insert)))'

(eval-after-load "smartparens" '(sp-local-pair 'emacs-lisp-mode "'" nil :actions nil))

(eval-after-load "smartparens" '(sp-local-pair 'twig-mode "{" nil :actions nil))
(eval-after-load "smartparens" '(sp-local-pair 'twig-mode "{{" "}}" :actions '(wrap insert)))
(eval-after-load "smartparens" '(sp-local-pair 'twig-mode "{%" "%}" :actions '(wrap insert)))
(eval-after-load "smartparens" '(sp-local-pair 'twig-mode "{#" "#}" :actions '(wrap insert)))
; Hmm, no workie.
(eval-after-load "twig-mode"      '(require 'smartparens-html))
(eval-after-load "smartparens" '(sp-local-tag  'twig-mode "<" "<_>" "</_>" :transform 'sp-match-sgml-tags :post-handlers '(sp-html-post-handler)))

; Try out http://www.emacswiki.org/emacs/MiniMap ?

; Solarized color scheme.
;(add-to-list 'load-path "~/.emacs.d/emacs-color-theme-solarized/")
;(require 'color-theme-solarized)

(add-to-list 'load-path "~/.emacs.d/emacs-powerline/")
(require 'powerline)

(add-to-list 'load-path "~/.emacs.d/drupal-mode/")
(require 'drupal-mode)

;; Redefine drupal-mode-beginning-of-line to use
;; back-to-indentation-or-beginning instead of beginning-of-line.
(defun drupal-mode-beginning-of-line (&optional n)
  "Move point to beginning of property value or to beginning of line.
The prefix argument N is passed directly to `beginning-of-line'.

This command is identical to `back-to-indentation-or-beginning' if not in a mode
derived from `conf-mode'.

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
    (back-to-indentation-or-beginning)))


; From http://www.emacswiki.org/emacs/BackToIndentationOrBeginning
; Go back to indentation or beginning of line.
(defun back-to-indentation-or-beginning () (interactive)
  (if (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line)))
(global-set-key (kbd "C-a") 'back-to-indentation-or-beginning)

;; http://www.emacswiki.org/emacs/AceJump  
(define-key global-map (kbd "S-SPC") 'ace-jump-mode)
;; 'global doesn't quite work for me, it fails oddly.
(setq ace-jump-mode-scope 'window)

(define-key global-map [delete] 'delete-char)
(define-key global-map [M-delete] 'kill-word)
(global-font-lock-mode t)

; For zap-up-to-char and possibly others.
(require 'misc)
(define-key global-map (kbd "M-Z") 'zap-up-to-char)

(require 'gtags)
(add-hook 'gtags-mode-hook
          #'(lambda()
	      (diminish 'gtags-mode "G ")
              (define-key gtags-mode-map [(meta \,)] 'helm-gtags-find-rtag)
              (define-key gtags-mode-map [(meta .)] 'helm-gtags-find-tag)
              (define-key gtags-mode-map [(meta *)] 'helm-gtags-pop-stack)
              (define-key gtags-mode-map [(control t)] nil)
              (define-key gtags-mode-map [mouse-2] nil)
              (define-key gtags-mode-map [mouse-3] nil)
              ))

; Activate flyspell and yas in magit commit buffer.
(defun xen-magit-log-edit-mode-hook ()
  (yas-minor-mode 1)
  (flyspell-mode)
)
(add-hook 'magit-log-edit-mode-hook 'xen-magit-log-edit-mode-hook)

; Add shortcut to open magit status buffer.
(global-set-key (kbd "C-c C-g") 'magit-status)

; Let projectile show the magit status buffer when switching to a project.
(defun xen-projectile-magit ()
  "Open magit when switching to project."
  (call-interactively 'magit-status)
  )

; I just want the branch to have the same name as origin.
(defun xen-magit-default-tracking-name
  (remote branch)
  "Use just the branch name for tracking branches."
  branch)

(helm-mode 1)
; Bugfix..
(require 'helm-aliases)
(require 'helm-files)
(define-key global-map (kbd "C-x C-f") 'helm-for-files)
(define-key global-map (kbd "C-x b") 'helm-buffers-list)

;; Show full paths in helm-projectile. Fix gleaned from
;; https://github.com/bbatsov/projectile/pull/193/files
(require 'helm-projectile)
(defun helm-projectile ()
  "Use projectile with Helm instead of ido."
  (interactive)
 (let ((helm-ff-transformer-show-only-basename nil))
  (helm :sources '(helm-c-source-projectile-files-list
                   helm-c-source-projectile-buffers-list
                   helm-c-source-projectile-recentf-list)
        :buffer "*helm projectile*"
        :prompt (projectile-prepend-project-name "pattern: "))
))


;; TODO: https://github.com/rolandwalker/fixmee

;; TODO: CamelCase <-> snake_case conversion:
;; https://gist.github.com/846766
;; https://github.com/emacsmirror/s

; Start EmacsRocks
; Really cool stuff: https://github.com/magnars
; From these:
; https://www.youtube.com/watch?v=p3Te_a-AGqM&feature=player_embedded#!
; http://emacsrocks.com/

(add-to-list 'load-path "~/.emacs.d/multiple-cursors/")
(require 'multiple-cursors)

(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-M-m") 'mc/mark-more-like-this-extended)
(global-set-key (kbd "C-*") 'mc/mark-all-like-this)
(global-set-key (kbd "C-%") 'mc/mark-all-in-region)
(global-set-key (kbd "C-=") 'mc/mark-all-like-this-dwim)

; todo: there's also http://www.emacswiki.org/emacs/iedit.el

(add-to-list 'load-path "~/.emacs.d/expand-region/")
(require 'expand-region)
(global-set-key (kbd "C-S-SPC") 'er/expand-region)

(defun xen-php-mark-next-accessor ()
  "Presumes that current symbol is already marked, skips over one
arrow and marks next symbol."
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

(defun php-mode-expansions ()
      (make-variable-buffer-local 'er/try-expand-list)
      (setq er/try-expand-list '(er/mark-word er/mark-symbol er/mark-symbol-with-prefix xen-php-mark-next-accessor xen-php-mark-method-call-or-array er/mark-comment er/mark-comment-block er/mark-inside-quotes er/mark-outside-quotes er/mark-inside-pairs er/mark-outside-pairs)))

(add-hook 'php-mode-hook 'php-mode-expansions)
(add-hook 'prog-mode-hook #'eldoc-mode)

; Writable grep buffer.
(add-to-list 'load-path "~/.emacs.d/wgrep/")
(require 'wgrep)

(require 'yasnippet)
;; Initialize yasnippet. It's enabled per mode.
(yas/reload-all) 

; End EmacsRocks

; Enable flycheck globally.
(add-hook 'after-init-hook #'global-flycheck-mode)

; Temporarily redefine eslint checker to use source-inplace, until
; it's fixed upstream.
(eval-after-load "flycheck" '(flycheck-define-checker javascript-eslint
  "A JavaScript syntax and style checker using eslint.

See URL `https://github.com/nzakas/eslint'."
  :command ("eslint" "--format=compact"
            (config-file "--config" flycheck-eslintrc)
            (option "--rulesdir" flycheck-eslint-rulesdir)
            source-inplace)
  :error-patterns
  ((warning line-start (file-name)
            ": line " line ", col " column ", Warning - " (message) line-end)
   (error line-start (file-name)
          ": line " line ", col " column ", Error - " (message) line-end))
  :modes (js-mode js2-mode js3-mode))
)

; Fixes flymake-phpcs, when the file has been accessed through a
; symlink in its path.
(defun flymake-create-temp-inplace (file-name prefix)
  (unless (stringp file-name)
    (error "Invalid file-name"))
  (or prefix
      (setq prefix "flymake"))
  (let* ((temp-name   (file-truename (concat (file-name-sans-extension file-name)
			      "_" prefix
			      (and (file-name-extension file-name)
				   (concat "." (file-name-extension file-name)))))))
    (flymake-log 3 "create-temp-inplace: file=%s temp=%s" file-name temp-name)
    temp-name))

; http://www.emacswiki.org/emacs/WinnerMode
(when (fboundp 'winner-mode)
  (winner-mode 1))
(global-set-key [(XF86Back)] 'winner-undo)
(global-set-key [(XF86Forward)] 'winner-redo)


; http://www.emacswiki.org/emacs/WindowResize
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

; http://www.emacswiki.org/emacs/WindMove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
; http://www.emacswiki.org/emacs/FrameMove
(require 'framemove)
(setq framemove-hook-into-windmove t)

; http://www.emacswiki.org/emacs/WholeLineOrRegion
(require 'whole-line-or-region)

; http://emacswiki.org/emacs/CopyingWholeLines
;; duplicate current line
(defun duplicate-current-line (&optional n)
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

(global-set-key (kbd "C-S-d") 'duplicate-current-line)

(setq x-select-enable-clipboard t) ; Makes yanking interact with the clipboard.

; As I never use C-v anyway, and its effect when i hit it confuses me,
; why not bind it to pasting from outside (like the middle button
; does)?
(global-set-key (kbd "C-v") 'xen-paste)
; Also CTRL Shift v (to mirror xen-copy), which is implicit in the
; above if not specifically bound, but let's make it explicit.
(global-set-key (kbd "S-C-v") 'xen-paste)
(defvar xen-paste-buffer "" "Local paste buffer.")
(defun xen-paste ()
 "Paste from outside."
 (interactive)
 ; x-selection-value returns nil when selection hasn't changed.
 (setq xen-paste-buffer (or (x-selection-value) xen-paste-buffer))
 (insert xen-paste-buffer)
)

; CRTL C is taken, so use CTRL Shift c like Gnome Terminal does, in
; order to limit the amount of different key combinations I should
; remember for the same thing.
(global-set-key (kbd "S-C-c") 'xen-copy)
(defun xen-copy (start end)
  "Copy to the outside."
  (interactive "r")
  (x-select-text (buffer-substring-no-properties start end))
)

; Don't switch to a frame already containing the selected buffer, but
; show the same buffer in a new frame.
(setq ido-default-buffer-method 'selected-window)

; todo: see if http://tuxicity.se/emacs/elisp/2010/03/14/drag-stuff-in-emacs.html is better
; http://www.emacswiki.org/emacs/MoveRegion
(defun move-region (start end n)
  "Move the current region up or down by N lines."
  (interactive "r\np")
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (let ((start (point)))
      (insert line-text)
      (setq deactivate-mark nil)
      (set-mark start))))

(defun move-region-up (start end n)
  "Move the current line up by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) -1 (- n))))

(defun move-region-down (start end n)
  "Move the current line down by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) 1 n)))

;; (global-set-key (kbd "M-<up>") 'move-region-up)
;; (global-set-key (kbd "M-<down>") 'move-region-down)
(global-set-key (kbd "S-M-<up>") 'move-region-up)
(global-set-key (kbd "S-M-<down>") 'move-region-down)

; http://www.emacswiki.org/emacs/SwapRegions
(defun swap-regions (beg1 end1 beg2 end2)
  "Swap region between BEG1 and END1 with region BEG2 and END2.

For the first region, mark the first region and set mark at
point.  The second region only needs to be marked normally.
Again, set the mark at the beginning and end of the first region,
then mark the second region with mark and point.

The order of the two regions in the buffer doesn't matter.
Either one can precede the other.  However, the regions can not
be swapped if they overlap.

All arguments can either be a number for a position in the buffer
or a marker."
  (interactive
   (if (< (length mark-ring) 2)
       (error "Not enough in mark-ring to swap a region")
     (let ((region (list (region-beginning) (region-end)))
	   (marks (sort (list (marker-position (car mark-ring))
			      (marker-position (cadr mark-ring)))
			'<)))
       (if (< (car region) (car marks))
	   (append region marks)
	 (append marks region)))))
  (if (or (and (< beg2 beg1) (< beg1 end2))
	  (and (< beg1 beg2) (< beg2 end1)))
      (error "Unable to swap overlapping regions")
      (save-excursion
	(insert
	 (prog1 (delete-and-extract-region beg2 end2)
	   (goto-char beg2)
	   (insert
	    (delete-and-extract-region beg1 end1))
	   (goto-char beg1))))))


; Todo: http://www.emacswiki.org/cgi-bin/wiki.pl?ColorTheme

;; In every buffer, the line which contains the cursor will be fully
;; highlighted
(global-hl-line-mode 1)

(iswitchb-mode)

(set-input-mode (car (current-input-mode))
        (nth 1 (current-input-mode))
        0)

; Hide some minor-modes I don't need to be told is active.
(diminish 'whole-line-or-region-mode "")
(diminish 'undo-tree-mode "")
(diminish 'auto-complete-mode "")
(diminish 'helm-mode "")
(diminish 'yas-minor-mode "")
(diminish 'abbrev-mode "")
; These aren't yet loaded.
(eval-after-load "smartparens" '(diminish 'smartparens-mode ""))
(eval-after-load "highlight-symbol" '(diminish 'highlight-symbol-mode ""))
(eval-after-load "eldoc" '(diminish 'eldoc-mode ""))
(eval-after-load "flyspell" '(diminish 'flyspell-mode ""))

(setq org-log-done 'time)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

; Setting 'globally':
; (setq-default show-trailing-whitespace t)


;; *** DRUPAL ***

(setq-default indent-tabs-mode nil)

(defun xen-open ()
  "Open new line, with proper indentation."
  (interactive)
  (call-interactively 'move-beginning-of-line)
  (call-interactively 'open-line)
  (indent-for-tab-command)
  )

(defun xen-paired-delete (arg &optional killp)
  "Test.."
  (interactive "*p\nP")
  (let*
      (
       (here (point))
       (newmark (xen-find-matching here))
       )
    (if (and newmark (< 0 newmark))
        (progn
          (save-excursion
            (if (< here newmark)
                (progn
                  (goto-char newmark)
                  (delete-backward-char 1)
                  (goto-char here)
                  (call-interactively 'delete-char)
                  (setq newmark (- newmark 2))
                  )
              (progn
                (goto-char newmark)
                (delete-char 1)
                (goto-char (- here 1))
                (call-interactively 'delete-char)
                )
              )
            )

          (push-mark newmark nil t) ; todo: doesn't work? or does it?
          )
      (call-interactively 'delete-char (list arg killp))
      )
    )
  )

(defun xen-paired-delete-backward (arg &optional killp)
  "Test.."
  (interactive "*p\nP")
  (let*
      (
       (here (point))
       (newmark (xen-find-matching (- here 1)))
       )
    (if (and newmark (< 0 newmark))
        (progn
          (save-excursion
            (if (< here newmark)
                (progn
                  (goto-char newmark)
                  (delete-backward-char 1)
                  (goto-char here)
                  (call-interactively 'delete-backward-char killp)
                  (setq newmark (- newmark 2))
                  )
              (progn
                (goto-char newmark)
                (delete-char 1)
                (goto-char (- here 1))
                (call-interactively 'delete-backward-char killp)
                )
              )
            )

          (push-mark newmark nil t)
          )
      (call-interactively 'backward-delete-char-untabify killp)
      )
    )
  )
; http://www.emacswiki.org/cgi-bin/wiki/delsel.el
(put 'xen-paired-delete-backward 'delete-selection 'supersede)
(put 'xen-paired-delete 'delete-selection 'supersede)

(defvar xen-delete-char-pairwise-alist '(
                                         (?" ?" 0)
                                         (?' ?' 0)
                                         (?{ ?} 1)
                                         (?} ?{ -1)
                                         (?( ?) 1)
                                         (?) ?( -1)
                                         (?[ ?] 1)
                                         (?] ?[ -1)
))

(defun xen-find-matching (pos)
  (let
      (newmark)
    (progn
      (save-excursion
        (progn
          (goto-char pos)
          (let*
              ((char (following-char))
               (pairing (assq char xen-delete-char-pairwise-alist))
               (deactivate-mark)
               )
            (if pairing
                (let
                    ((apair (nth 0 pairing))
                     (bpair (nth 1 pairing))
                     (direction (nth 2 pairing)))
                  (if (= direction 1)
                      (progn ; forward
                                        ; TODO scan-lists chokes on mismached..
                        (setq newmark (scan-lists pos 1 0))
(message (string newmark))
                        (if (= (char-before newmark) bpair) () (setq newmark nil))
                        )

                    (if (= direction -1)
                        (progn ; backwards
                          (setq newmark (scan-lists pos -1 1))
                          (if (= (char-after newmark) bpair) () (setq newmark nil))
                          )
                      (progn ; figure it out
                        (let (
                              (f (get-text-property (- pos 1) 'face))
                              (f2 (get-text-property (+ pos 1) 'face))
                              )
                          (progn
                                        ; TODO check the other direction and cop out if it's comment/string too. Think it's done
                                        ; TODO doesn't deal well with backspace in the middle of ''. Should be fixed by killing forward-char below.

                            (setq direction (if (memq f xen-prog-text-faces)
                                                 (progn
                                                   (if (memq f2 xen-prog-text-faces) 0 -1) ; Check the other direction and cop out if it too is a comment
                                                   )
                                              1
                                              )
                                  )
                            ;(message (string direction))
                            )
                          )
                        (setq newmark
                              (if (= direction 1)
                                  (progn
                                    ;(forward-char)
                                    (re-search-forward (concat "[^\\]" (list bpair))))
                                (if (= direction -1)
                                    (progn
                                      (+ (re-search-backward (concat "[^\\]" (list bpair))) 1)
                                      )
                                  (progn ; 0 case, cop out
                                   (setq newmark nil)
                                   )
                                  )
                                )
                              )
                        )
                      )
                    )
                  )
              )
            )
          )
        newmark
        )
      )
    )
  )

(defun xen-char-syntax ()
"Shows the syntax class of the character following point."
(interactive)
(message (char-to-string (char-syntax (char-after)))))

(defun xen-tab ()
  "Indent if on whitespace or do nothing (auto-complete/company and yasnippet will attach themselves.)."
  (interactive "*")
  (if (or (bolp) ; Beginning of line
          (region-active-p) ; We have an active region
          (eq (char-syntax (char-before)) ?\ ) ; Or whitespace
          )
      (indent-for-tab-command)
    )
  )

; This face hackery is stolen from flyspell.
(defvar xen-prog-text-faces
  '(font-lock-string-face font-lock-comment-face font-lock-doc-face)
  "Faces corresponding to text in programming-mode buffers.")

(defun xen-coding-common-bindings ()
  (local-set-key (kbd "C-o") 'xen-open)
  (local-set-key [return] 'newline-and-indent)
  (local-set-key [backspace] 'xen-paired-delete-backward)
  (local-set-key [delete] 'xen-paired-delete)
  (local-set-key [tab] 'xen-tab)
  (local-set-key [S-iso-lefttab] 'indent-for-tab-command)
  (local-set-key [C-tab] 'helm-browse-code)
  (highlight-symbol-mode)
  (local-set-key (kbd "M-<left>") 'highlight-symbol-prev)
  (local-set-key (kbd "M-<right>") 'highlight-symbol-next)
  (local-set-key (kbd "M-<up>") 'flycheck-previous-error)
  (local-set-key (kbd "M-<down>") 'flycheck-next-error)
  (flyspell-prog-mode)
)

(add-hook 'emacs-lisp-mode-hook 
(lambda  ()
  (xen-coding-common-bindings)
  (yas-minor-mode 1)
  ))

(defun my-php-mode-hook ()
  (xen-coding-common-bindings)
  ;; Work around bug in Emacs 23.3.1 cc-mode c-fill-paragraph
  ;; http://superuser.com/questions/250442/fixing-c-fill-paragraph-with-comments-in-emacs-23-2-1
  (local-set-key (kbd "M-q") 'fill-paragraph)
  (modify-syntax-entry ?_ "_" php-mode-syntax-table)
  (yas-minor-mode 1)
  (gtags-mode)
  (column-enforce-mode)
  (diminish 'column-enforce-mode "")
  )
(add-hook 'php-mode-hook 'my-php-mode-hook)

(add-hook 'js-mode-hook 'xen-js-mode-hook)
(defun xen-js-mode-hook ()
  (xen-coding-common-bindings)
  (yas-minor-mode 1)
  (column-enforce-mode)
  (diminish 'column-enforce-mode "")
  )

(add-hook 'js2-mode-hook 'xen-js2-mode-hook)
(defun xen-js2-mode-hook ()
  (xen-coding-common-bindings)
  (yas-minor-mode 1)
  (column-enforce-mode)
  (diminish 'column-enforce-mode "")
  )

(add-hook 'css-mode-hook 'xen-css-mode-hook)
(defun xen-css-mode-hook ()
  (xen-coding-common-bindings)
  (yas-minor-mode 1)
  (column-enforce-mode)
  (diminish 'column-enforce-mode "")
  )

; To make fill-paragraph work with doxygen comments.
(defun php-doc-paragraph-boundaries ()
  (setq paragraph-separate "^[ \t]*\\(\\(/[/\\*]+\\)\\|\\(\\*+/\\)\\|\\(\\*?\\)\\|\\(\\*?[ \t]*@[[:alpha:]]+\\([ \t]+.*\\)?\\)\\)[ \t]*$")
  (setq paragraph-start (symbol-value 'paragraph-separate)))

(add-hook 'php-mode-user-hook 'php-doc-paragraph-boundaries)

(global-set-key [?\C-x ?\C-b] 'buffer-menu)
(defun my-ruby-mode-hook ()
  (xen-coding-common-bindings)
  (yas-minor-mode 1)
  )
(add-hook 'ruby-mode-hook 'my-ruby-mode-hook)

;; Properly handle annotations in java-mode.
(require 'java-mode-indent-annotations)
(add-hook 'java-mode-hook 'java-mode-indent-annotations-setup)

;; *** IDO ***

(setq ido-enable-flex-matching t)

; Check: http://www.xsteve.at/prg/emacs/power-user-tips.html
; (require 'ffap)
; (ffap-bindings)

(setq next-line-add-newlines nil)
(setq hs-minor-mode-hook nil)

;; Relocate and load customs (so we don't clutter init.el with them).
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file)
