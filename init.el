;;; init.el --- Xens emacs configuration.
;;; Commentary:

;; Handy trick:
;; (set-face-attribute 'default nil :height 140)

;; Handy variables for line (non-)wrapping:
;; trucante-lines
;; word-wrap
;; Also, look into adaptive-wrap (mentioned here: http://emacswiki.org/emacs/LineWrap )

;; Take a look at http://www.emacswiki.org/emacs/MarkCommands
;; Maybe ressurrect https://github.com/xendk/dotemacs/commit/4d718daf386ae329e9d65ec90780f0fdc55f138e

;; really should try out https://github.com/jwiegley/use-package
;;; Code:

; Quick debugging:
; (toggle-debug-on-error)

;; Start server if not already running.
(require 'server)
(if (not (server-running-p))
    (progn
      (server-start)
      (add-hook 'kill-emacs-hook (lambda () (server-start 1)))
      )
  )

;;; Configuration.
;; Relocate and load customs (so we don't clutter init.el with them).
;; Loading them first so colors, faces and menu/toolbar/scrollbar is
;; removed early.
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file)

(setq load-path (cons "~/.emacs.d/lib/" load-path))

;;; Bindings.
;(require 'bind-key)
;; TODO: use bind-key.
(define-key global-map [delete] 'delete-char)
(define-key global-map [M-delete] 'kill-word)
(define-key global-map (kbd "C-S-Z") 'repeat)

;; Don't iconify on C-z.
(global-unset-key (kbd "C-z"))
;; (when (display-graphic-p)
;;   (unbind-key "C-z"))

(global-set-key [f11] 'xen-toggle-fullscreen)
(global-set-key [f12] 'xen-big-fringe-mode)
(global-set-key (kbd "C-a") 'xen-back-to-indentation-or-beginning)

;; Add shortcut to open magit status buffer.
(global-set-key (kbd "C-c C-g") 'magit-status)
(define-key global-map (kbd "C-x C-f") 'xen-find-file)

;; http://www.emacswiki.org/emacs/WindowResize
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(global-set-key (kbd "C-S-d") 'xen-duplicate-current-line)

; As I never use C-v anyway, and its effect when i hit it confuses me,
; why not bind it to pasting from outside (like the middle button
; does)?
(global-set-key (kbd "C-v") 'xen-paste)
; Also CTRL Shift v (to mirror xen-copy), which is implicit in the
; above if not specifically bound, but let's make it explicit.
(global-set-key (kbd "S-C-v") 'xen-paste)

; CRTL C is taken, so use CTRL Shift c like Gnome Terminal does, in
; order to limit the amount of different key combinations I should
; remember for the same thing.
(global-set-key (kbd "S-C-c") 'xen-copy)

;;; Aliases
;; I'm grown up, I can manage using y/n for even destructive commands.
(defalias 'yes-or-no-p 'y-or-n-p)


;;; Packages.
;;;; Initialize package system.
(require 'package)
(add-to-list 'package-archives
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;;; Use use-package for loading packages.
(require 'use-package)

(use-package ace-jump-mode
  :bind ("S-SPC" . ace-jump-mode))

(use-package browse-kill-ring
  :init (browse-kill-ring-default-keybindings))

(use-package diff-hl
  :config (global-diff-hl-mode))

(use-package drag-stuff
  :config (progn
            (setq drag-stuff-modifier '(meta shift))
            (drag-stuff-global-mode)))

(use-package drupal-mode
  :load-path "~/.emacs.d/drupal-mode/"
  :config (progn
            (bind-key "C-a" 'xen-drupal-mode-beginning-of-line drupal-mode-map)))

(use-package expand-region
  :bind ("C-S-SPC" . er/expand-region))

(use-package flycheck
  ;; Enable flycheck globally.
  :config (add-hook 'after-init-hook #'global-flycheck-mode)
)

(use-package google-this
  :diminish google-this-mode
  :init (google-this-mode))

(use-package gtags
  :diminish "G "
  :config (progn
            ;; Adjust the keymap.
            (bind-key "M-," 'helm-gtags-find-rtag gtags-mode-map)
            (bind-key "M-." 'helm-gtags-find-tag gtags-mode-map)
            (bind-key "M-*" 'helm-gtags-pop-stack gtags-mode-map)
            ;; Unbind some keys.
            (unbind-key "<mouse-2>" gtags-mode-map)
            (unbind-key "<mouse-3>" gtags-mode-map)))

(use-package helm
  :diminish helm-mode
  :commands helm-mode
  :init (helm-mode 1)
  :bind (("C-x b" . helm-buffers-list)
         ("<C-S-iso-lefttab>" . helm-swoop))
  :config (progn
            ;; Ressucect helm-browse-code
            (load (locate-user-emacs-file "helm-compat.el"))))

(use-package helm-projectile
  :config (progn
            (eval-after-load "projectile" '(bind-key "p" 'helm-projectile-switch-project projectile-command-map)))
  )

(use-package hl-line
  :config (global-hl-line-mode))

(use-package magit
  :diminish magit-auto-revert-mode
  :init (add-hook 'magit-log-edit-mode-hook 'xen-magit-log-edit-mode-hook)

)

; Add git flow extension.
(use-package magit-gitflow
  :init (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))

; Add github pull request extension.
(use-package magit-gh-pulls
  :init (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))

(use-package multiple-cursors
  :bind (
         ("C-<" . mc/mark-previous-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-M-m" . mc/mark-more-like-this-extended)
         ("C-*" . mc/mark-all-like-this)
         ("C-%" . mc/mark-all-in-region)
         ("C-=" . mc/mark-all-like-this-dwim)))

(use-package navorski
  :init (nav/defterminal
          guard
          :program-path "/usr/local/bin/guard"
          :cwd (lambda (path) (locate-dominating-file path "Guardfile"))
          :interactive t
          ))


(use-package projectile
  :commands projectile-project-p
  :diminish projectile-mode
  :init (projectile-global-mode)
  :config
  (progn
    (setq projectile-cache-file "~/.emacs.d/.projectile.cache")
    (setq projectile-known-projects-file
          "~/.emacs.d/.projectile-bookmarks.eld")))

(use-package smartparens-config
  :diminish smartparens-mode
  :init (progn
          (smartparens-global-mode 1)
          (show-smartparens-global-mode 1))
  :config (progn
            (sp-pair "'" nil :unless '(sp-point-after-word-p))
            ;; When pressing return as the first thing after inserting
            ;; a { or (, add another and indent.
            (sp-local-pair 'php-mode "{" nil :post-handlers '(("||\n[i]" "<return>")))
            (sp-local-pair 'php-mode "(" nil :post-handlers '(("||\n[i]" "<return>")))

            (sp-local-pair 'css-mode "/*" "*/" :actions '(wrap insert))'

            ;; Don't autopair ' in lisp.
            (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)

            (sp-local-pair 'twig-mode "{" nil :actions nil)
            (sp-local-pair 'twig-mode "{{" "}}" :actions '(wrap insert))
            (sp-local-pair 'twig-mode "{%" "%}" :actions '(wrap insert))
            (sp-local-pair 'twig-mode "{#" "#}" :actions '(wrap insert))
            ;; Hmm, no workie.
            ;; (eval-after-load "twig-mode"      '(require 'smartparens-html))
            ;; (eval-after-load "smartparens" '(sp-local-tag  'twig-mode "<" "<_>" "</_>" :transform 'sp-match-sgml-tags :post-handlers '(sp-html-post-handler)))
            ;; (require 'smartparens-html)
            )
  )

(use-package undo-tree
  :diminish undo-tree-mode
  :init (progn
          (global-undo-tree-mode)))

(use-package xen
  :load-path "~/.emacs.d/xen/")

(use-package yasnippet
  :diminish yas-minor-mode
  :idle
  (yas-reload-all))


;;; Old stuff in need of cleaning up.

(defadvice show-paren-function (after my-echo-paren-matching-line activate)
  "If a matching paren is off-screen, echo the matching line."
  (when (char-equal (char-syntax (char-before (point))) ?\))
    (let ((matching-text (blink-matching-open)))
      (when matching-text
        (message matching-text)))))

; Try out http://www.emacswiki.org/emacs/MiniMap ?

; Solarized color scheme.
;(add-to-list 'load-path "~/.emacs.d/emacs-color-theme-solarized/")
;(require 'color-theme-solarized)


;; TODO: https://github.com/rolandwalker/fixmee

;; TODO: CamelCase <-> snake_case conversion:
;; https://gist.github.com/846766
;; https://github.com/emacsmirror/s

; Start EmacsRocks
; Really cool stuff: https://github.com/magnars
; From these:
; https://www.youtube.com/watch?v=p3Te_a-AGqM&feature=player_embedded#!
; http://emacsrocks.com/


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

(defun php-mode-expansions ()
      (make-variable-buffer-local 'er/try-expand-list)
      (setq er/try-expand-list '(er/mark-word er/mark-symbol er/mark-symbol-with-prefix xen-php-mark-next-accessor xen-php-mark-method-call-or-array er/mark-comment er/mark-comment-block er/mark-inside-quotes er/mark-outside-quotes er/mark-inside-pairs er/mark-outside-pairs)))

(add-hook 'php-mode-hook 'php-mode-expansions)
(add-hook 'prog-mode-hook #'eldoc-mode)

(defun xen-mark-lines ()
  "Mark the current line, or expand the selection to another line."
  (interactive)
  (let ((start (point)))
    (progn
      (exchange-point-and-mark)
      (end-of-line)
      (forward-char)
      (set-mark (point))
      (goto-char start)
      (beginning-of-line)
      )
    ))

; Advice er/expand-region to only expand regions when the trigger key is l.
(defadvice er/expand-region (around expand-lines activate)
  (let* ((repeat-key (event-basic-type last-input-event))
         (repeat-key-str (single-key-description repeat-key))) 
           (let ((er/try-expand-list (if (equal repeat-key-str "l") '(xen-mark-lines) er/try-expand-list)))
             ad-do-it
             )))

; Bind er/expand region to our boobytrapped key.
(global-set-key (kbd "C-S-l") 'er/expand-region)
(setq er--show-expansion-message nil)

; Writable grep buffer.
(require 'wgrep)

; End EmacsRocks

(eval-after-load 'ace-jump-zap
  '(progn
    (define-key global-map (kbd "M-z") 'ace-jump-zap-to-char)
    (define-key global-map (kbd "M-Z") 'ace-jump-zap-up-to-char)))
(require 'ace-jump-zap)


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

; Trying realgud.
(add-to-list 'load-path "~/.emacs.d/emacs-dbgr/")

; http://www.emacswiki.org/emacs/WinnerMode
(when (fboundp 'winner-mode)
  (winner-mode 1))
(global-set-key [(XF86Back)] 'winner-undo)
(global-set-key [(XF86Forward)] 'winner-redo)



; http://www.emacswiki.org/emacs/WindMove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
 ;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)
; http://www.emacswiki.org/emacs/FrameMove
(require 'framemove)
(setq framemove-hook-into-windmove t)

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

; todo: http://stackoverflow.com/questions/892378/how-do-i-fix-the-cursor-to-the-middle-of-the-screen-in-emacs-so-that-the-page-m
; http://www.emacswiki.org/emacs/centered-cursor-mode.el

; Figure out how to ensure 80 cols with this.
; (global-linum-mode 1)

;; In every buffer, the line which contains the cursor will be fully
;; highlighted



; These aren't yet loaded.
(eval-after-load "highlight-symbol" '(diminish 'highlight-symbol-mode ""))
(eval-after-load "eldoc" '(diminish 'eldoc-mode ""))
(eval-after-load "flyspell" '(diminish 'flyspell-mode ""))
(eval-after-load "company" '(diminish 'company-mode ""))

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; *** DRUPAL ***

(defun xen-open ()
  "Open new line, with proper indentation."
  (interactive)
  (call-interactively 'move-beginning-of-line)
  (call-interactively 'open-line)
  (indent-for-tab-command)
  )

(defun xen-paired-delete (arg &optional killp)
  "Deletes the matching pair if deleting a pair."
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
  "Deletes the matching pair if deleting a pair."
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

(add-to-list 'load-path "~/.emacs.d/geben/")
(autoload 'geben "geben" "DBGp protocol frontend, a script debugger" t)

;; Geben hackery.
(defun geben-open ()
  "Open the current buffer in geben."
  (interactive)
  (progn
    (let ((geben-current-session (car geben-sessions)))
      (geben-open-file (geben-source-fileuri geben-current-session (buffer-file-name)))
      )
    )
  )

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


(autoload 'multi-term-dedicated-exist-p "multi-term")
(defun multi-term-dedicated-toggle-and-select ()
  "Toggle dedicated `multi-term' window and select it."
  (interactive)
  (if (multi-term-dedicated-exist-p)
      (multi-term-dedicated-close)
    (progn (multi-term-dedicated-open) (multi-term-dedicated-select))))

(global-set-key (kbd "C-!") 'multi-term-dedicated-toggle-and-select)

;; Get paste working in (multi-)term-mode.
(add-hook 'term-mode-hook (lambda ()
                            (define-key term-raw-map (kbd "C-y") 'term-paste)
                            (define-key term-raw-map (kbd "C-v") 'xen-paste-term)
                            (define-key term-raw-map (kbd "S-C-v") 'xen-paste-term)
                            ))


; To make fill-paragraph work with doxygen comments.
;; (defun php-doc-paragraph-boundaries ()
;;   (setq paragraph-separate "^[ \t]*\\(\\(/[/\\*]+\\)\\|\\(\\*+/\\)\\|\\(\\*?\\)\\|\\(\\*?[ \t]*@[[:alpha:]]+\\([ \t]+.*\\)?\\)\\)[ \t]*$")
;;   (setq paragraph-start (symbol-value 'paragraph-separate)))

;; (add-hook 'php-mode-user-hook 'php-doc-paragraph-boundaries)

(global-set-key [?\C-x ?\C-b] 'buffer-menu)
(defun my-ruby-mode-hook ()
  (xen-coding-common-bindings)
  (yas-minor-mode 1)
  )
(add-hook 'ruby-mode-hook 'my-ruby-mode-hook)

;; Properly handle annotations in java-mode.
(require 'java-mode-indent-annotations)
(add-hook 'java-mode-hook 'java-mode-indent-annotations-setup)

(provide 'init)
;;; init.el ends here
