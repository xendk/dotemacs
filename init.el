;;; init.el --- Xens emacs configuration.  -*- flycheck-emacs-lisp-load-path: inherit; -*-
;;; Commentary:

;; My Emacs init.el.

;;; Code:

;; Start server if not already running.
(require 'server)
(defvar xen-primary (not (server-running-p)) "Whether this Emacs is \"Primary\".")
(if xen-primary
    (progn
      (server-start)
      (add-hook 'kill-emacs-hook (lambda () (server-start 1))))
  ;; If it's running, this is the second instance of Emacs, which is
  ;; most likely used for testing something, so debug on error.
  (toggle-debug-on-error))



;; https://git.sr.ht/~pkal/emacs-init/tree/master/item/init.el
;; Also: https://github.com/progfolio/.emacs.d/blob/master/init.org
;; Use +notation.
;; Heavy: https://github.com/progfolio/.emacs.d/blob/master/init.org#custom-set-variables
;; Many good things: https://git.acdw.net/emacs/tree/?id=3e78d1f8ca5b100f39577790614433398bc6a422
;; Core emacs stuff. Some parts was nicked from https://github.com/grettke/lolsmacs
(setup emacs
  (:global
   ;; Used to M-DEL deleting a word.
   "M-<delete>" kill-word
   ;; Don't iconify on C-z.
   "C-z" nil
   ;; Scrolling on C-v confuses me when my muscle memory tries to use it as paste.
   "C-v" nil
   ;; Take out it's mate for consistency.
   "M-v" nil
   ;; Quickly delete the current buffer.
   "C-x C-k" kill-current-buffer
   ;; And often I want to kill the window too.
   "C-x K" kill-buffer-and-window
   ;; Horizontal scrolling on trackpad produces these, which makes Emacs
   ;; print warnings about undefined keys. I don't want to do anything on
   ;; horizontal scroll.
   "<mouse-6>" ignore
   "<mouse-7>" ignore
   ;; Window resizing.
   "S-C-<left>" shrink-window-horizontally
   "S-C-<right>" enlarge-window-horizontally
   "S-C-<down>" shrink-window
   "S-C-<up>" enlarge-window)

  (:with-mode prog-mode
    (:hook eldoc-mode) ; TODO: move to eldoc.
    (:hook (lambda () (setq-local comment-auto-fill-only-comments t)
             (auto-fill-mode))))

  (:with-hook compilation-filter-hook
    (:hook ansi-color-compilation-filter))

  ;; Some variables set above is duplicated here, should be cleaned up.
  (:option
   ;; Better ANSI colors
   ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold]
   ;; Move auth-sources to XDG_CONFIG
   auth-sources '("~/.config/emacs/authinfo.gpg")
   ;; Only scroll current line
   auto-hscroll-mode 'current-line
   ;; Don't place backups next to the original file, but move them to
   ;; <user-emacs-directory>/backups
   backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
   ;; Also delete newlines on backward-delete-char-untabify
   backward-delete-char-untabify-method 'all
   ;; Use browse-url-generic-program
   browse-url-browser-function 'browse-url-generic
   ;; Set a working browser
   browse-url-generic-program "sensible-browser"
   ;; Use indent from c-style
   c-basic-offset 'set-from-style
   ;; Set c-styles
   c-default-style '((java-mode . "java") (awk-mode . "awk") (php-mod . "psr2") (other . "gnu"))
   ;; We've activated `ansi-color-compilation-filter', so tell
   ;; compilation programs colors are OK
   compilation-environment '("TERM=xterm-256color")
   ;; Makes Corfu tab-complete on single matches work
   completion-cycle-threshold 3
   ;; Lock files mess with watchers, and I don't have much use for it
   ;; on a single-user system.
   create-lockfiles nil
   ;; Echo keystrokes quickly
   echo-keystrokes 0.02
   ediff-split-window-function 'split-window-horizontally
   ;; Use a single frame for all ediff windows
   ediff-window-setup-function 'ediff-setup-windows-plain
   enable-recursive-minibuffers t
   ;; Print everything when eval'ing
   eval-expression-print-level nil
   ;; Makes it easier to dismiss them with q
   help-window-select t
   history-delete-duplicates t
   ;; Increase margin for horizontal scroll
   hscroll-margin 15
   ;; Don't use tabs for indentation
   indent-tabs-mode nil
   inhibit-startup-screen t
   ;; Show counts in isearch
   isearch-lazy-count t
   ;; Seconds idle before starting to fontify in the background
   jit-lock-stealth-time 10
   js-indent-level 2
   ;; Counts before the search string messes with readability...
   lazy-count-prefix-format nil
   ;; ...so show them as suffix.
   lazy-count-suffix-format " [%s of %s]"
   ;; Don't move by visual lines
   line-move-visual nil
   menu-bar-mode nil
   ;; Make scrollwheel behave more like in other apps
   mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control)))
   ;; Yank at point, not pointer position when mouse-yanking
   mouse-yank-at-point t
   ;; Prefer newer .el file over .elc
   load-prefer-newer t
   ;; Give mini-buffers a bit more room
   max-mini-window-height 0.33
   ;; Cache passwords a bit longer
   password-cache-expiry 3600
   ;; Highlight matches when query-replacing
   query-replace-highlight t
   ;; Save more items in recent files
   recentf-max-saved-items 500
   ;; Allow inherit for flycheck-emacs-lisp-load-path for my init.el
   safe-local-variable-values '((flycheck-emacs-lisp-load-path . inherit))
   ;; Don't lose clips from other programs
   save-interprogram-paste-before-kill t
   scroll-bar-mode nil
   ;; Scroll linewise rather than jumping
   scroll-conservatively 2
   ;; Keep a margin to top/bottom of window
   scroll-margin 5
   ;; Don't jump around when scrolling
   scroll-preserve-screen-position t
   ;; Don't require double space after period to consider it a sentence
   sentence-end-double-space nil
   ;; Allow for repeatedly popping the mark using C-SPC
   set-mark-command-repeat-pop t
   ;; Don't use shift + cursors to mark regions
   shift-select-mode nil
   ;; Want to stick to end of line
   track-eol t
   ;; Re-uniquify buffers after killing some
   uniquify-after-kill-buffer-p t
   ;; Use appended brackets for file path
   uniquify-buffer-name-style 'post-forward-angle-brackets
   ;; Add a slash to directory buffers
   uniquify-trailing-separator-p t
   ;; Don't require confirmation on cookies
   url-cookie-confirmation 'nil
   ;; I'm grown up, I can manage using y/n for even destructive commands.
   use-short-answers t
   ;; Disable (mouse) dialogs, something is confusing emacs making it think some commands were mouse initiated
   use-dialog-box nil
   ;; Set email address
   user-mail-address "xen@xen.dk"
   ;; Only show warnings buffer on errors
   warning-minimum-level :error
   ;; Allow C-x C-q to change permissions too
   wdired-allow-to-change-permissions t
   ;; Make tabs more visible
   whitespace-style '(face tabs tab-mark))

  ;; TODO: Handle modes like this: https://git.acdw.net/emacs/tree/lisp/+emacs.el?id=3e78d1f8ca5b100f39577790614433398bc6a422#n187
  ;; Disable tool-bar-mode.
  (tool-bar-mode 0)
  ;; Show column number in mode-line.
  (column-number-mode)
  ;; Offer to automatically populate some new files.
  (auto-insert-mode)
  ;; Enable whitespace mode globally.
  (global-whitespace-mode)
  ;; Show minibuffer depth when using recursive minibuffers.
  (minibuffer-depth-indicate-mode)
  ;; Display tabs with a more specific character.
  (defvar whitespace-display-mappings)
  (setf
   (cdr (assoc 'tab-mark whitespace-display-mappings))
   '(?\t [?↹ ?\t] [?\t]))
  ;; Protect scratch buffer against accidental killing.
  (with-current-buffer "*scratch*"
    (emacs-lock-mode 'kill))
  ;; https://www.masteringemacs.org/article/demystifying-emacs-window-manager
  ;; explains how to make display-buffer display things like you want.
  (add-to-list 'display-buffer-alist
               ;; Make help buffers reuse single window.
               '("\\*Help\\*"
                 (display-buffer-reuse-window display-buffer-pop-up-window)
                 (reusable-frames . visible)))
  ;; `ansi-color-compilation-filter' interprets ANSI codes in the region
  ;; from `compilation-filter-start' to point. But
  ;; `compilation-filter' has already handled backspaces and carriage
  ;; returns, so the start of the newly inserted output might actually
  ;; be before `compilation-filter-start'. Hack around it by moving
  ;; `compilation-filter-start' back to the beginning of the line it
  ;; is within.
  (define-advice ansi-color-compilation-filter (:around (orig-func) xen-ansi-color-compilation-filter)
    (let ((compilation-filter-start
           (save-excursion
             (goto-char compilation-filter-start)
             (beginning-of-line)
             (point))))
      (funcall orig-func)))
  ;; Emacs 24 changed the region highlight from a hackery face thingy
  ;; to a proper overlay. Which is fine apart from giving it a nil
  ;; priority which puts it below pretty much everything else. So we
  ;; redefine the redisplay-highlight-region-function to give the
  ;; overlay a higher priority.
  ;;
  ;; Further inspiration:
  ;; https://www.reddit.com/r/emacs/comments/345by9/having_the_background_face_for_selection_region/
  (setq redisplay-highlight-region-function
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



;;; Appearance

(setup doom-themes
  (:elpaca t)
  (:option
   ;; Make the comments and modeline brighter.
   doom-nord-light-brighter-comments t
   doom-nord-light-brighter-modeline t)

  ;; Load the theme
  (load-theme 'doom-nord-light t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)

  (:face
   ;; Remove background color on comments (why does setting
   ;; doom-nord-comment-bg to nil not work?).
   'font-lock-comment-face '(:background unspecified)
   ;; White background for inactive mode-line is no-go for me.
   'mode-line-inactive `(:background ,(doom-lighten (doom-color 'modeline-bg) .3))
   ;; Make symbol highlight and region highlights a lighter version of the region.
   'region-occurrences-highlighter-face
   `( :inverse-video unspecified
      :background ,(doom-blend (doom-color 'region) (doom-color 'bg) 0.50))
   'highlight-symbol-face
   `(:background ,(doom-blend (doom-color 'region) (doom-color 'bg) 0.50))))

;; TODO: Needs update, but new version changes the `check' segment,
;; and I liked the old one better. Forward port it.
(setup doom-modeline
  (:elpaca t)
  (:hook-into elpaca-after-init)
  (:hook (lambda ()
           (doom-modeline-set-modeline 'xen-main 'default)))
  (:when-loaded
    (:option
     doom-modeline-buffer-file-name-style 'truncate-except-project
     (prepend* doom-modeline-mode-alist) '((vterm-mode . xen-minimal)
                                           (lisp-interaction-mode . xen-minimal)
                                           (dashboard-mode . xen-minimal)
                                           (help-mode . xen-minimal)))

    ;; Define an alternative 'main that has `check' after `buffer-info'
    ;; so it's visible even if the mode-line gets truncated.
    (doom-modeline-def-modeline 'xen-main
      '( eldoc bar workspace-name window-number modals matches
         follow buffer-info check remote-host buffer-position word-count
         parrot selection-info )
      '( compilation objed-state misc-info persp-name battery grip
         irc mu4e gnus github debug repl lsp minor-modes input-method
         indent-info buffer-encoding major-mode process vcs time ))

    ;; Define a custom mode-line segment.
    (doom-modeline-def-segment buffer-info-simple-icon
      "As `buffer-info', but without state icon."
      (concat
       (doom-modeline-spc)
       (doom-modeline--buffer-mode-icon)
       (doom-modeline--buffer-name)))

    ;; As 'minimal, but without buffer state icon.
    (doom-modeline-def-modeline 'xen-minimal
      '(bar matches buffer-info-simple-icon)
      '(media-info major-mode " "))

    ;; Set the mode-line of special buffers that existed before
    ;; doom-modeline was loaded.
    (require 'seq)
    (let ((buffers (seq-filter
                    (lambda (buffer)
                      (with-current-buffer buffer
                        (derived-mode-p 'special-mode)))
                    (buffer-list))))
      (dolist (buffer buffers)
        (with-current-buffer buffer
          (doom-modeline-set-modeline 'xen-minimal))))))

(setup nerd-icons
  (:elpaca t))

;; Remove when we update dashboard
(setup all-the-icons
  (:elpaca t)
  (:when-loaded
    (add-to-list 'all-the-icons-mode-icon-alist '(vterm-mode all-the-icons-octicon "terminal" :v-adjust 0.2 :height 1.0))))

(setup hl-line
  (:require +hl-line)
  (:with-function +hl-line-mode
    (:hook-into after-change-major-mode)))

(setup display-line-numbers-mode
  (:require +display-line-numbers-mode)
  (:option
   ;; Only grow room for line numbers
   display-line-numbers-grow-only t
   ;; Show major line ever 20 lines
   display-line-numbers-major-tick 20
   ;; Show minor line every 5 lines
   display-line-numbers-minor-tick 5)
  (:face
   'line-number-major-tick '(:foreground "dark gray" :background unspecified)
   'line-number-minor-tick '(:foreground "dim gray" :background unspecified))
  (:with-function +display-line-numbers-mode
    (:hook-into prog-mode)))

(setup highlight-symbol
  (:also-load +highlight-symbol)
  (:elpaca t)
  (:option
   highlight-symbol-idle-delay 0.5)
  (:global
   "M-<left>" highlight-symbol-prev
   "M-<right>" highlight-symbol-next)
  (:hook-into emacs-lisp-mode php-mode css-mode js-mode enh-ruby-mode)

  (add-hook 'activate-mark-hook '+highlight-symbol-mode-deactivate)
  (add-hook 'deactivate-mark-hook '+highlight-symbol-mode-reactivate))

(setup page-break-lines
  (:elpaca t)
  (:hook-into emacs-lisp-mode))

(setup region-occurrences-highlighter
  (:elpaca t)
  (:hook-into emacs-lisp-mode php-mode css-mode js-mode enh-ruby-mode)
  (:with-map region-occurrences-highlighter-nav-mode-map
    (:bind
     "M-<up>" region-occurrences-highlighter-prev
     "M-<down>" region-occurrences-highlighter-next)))

(setup rainbow-mode
  (:elpaca t)
  (:also-load +rainbow-mode)
  (advice-add #'rainbow-colorize-match :override #'+rainbow-colorize-match)
  (advice-add #'rainbow-turn-off :after #'+rainbow-clear-overlays)
  (:hook-into css-mode))

(setup diff-hl
  (:elpaca t)
  (global-diff-hl-mode))

(setup column-enforce-mode
  (:elpaca t)
  (:hook-into drupal-mode)
  (with-eval-after-load 'doom-themes
    (:face 'column-enforce-face `( :inherit nil :underline nil
                                   :background ,(doom-lighten 'warning .75)))))

(setup dimmer
  (:elpaca t)
  (:when-loaded
    (:option
     dimmer-fraction 0.30
     (append dimmer-buffer-exclusion-regexps) "^ \\*corfu\\*$"))
  (dimmer-configure-which-key)
  (dimmer-configure-magit)
  (dimmer-configure-org)
  ;; TODO: How to detect active vertico, so buffers don't get dimmed
  ;; while it's active? Something to add to
  ;; dimmer-prevent-dimming-predicates.
  (dimmer-mode))

(setup dashboard
  (:elpaca t)
  (:require dashboard page-break-lines)
  (:option
   ;; Use page-break-lines-mode
   dashboard-page-separator "\n\f\n"
   ;; Use project backend
   dashboard-projects-backend 'project-el
   ;; Use the project switch project command.
   dashboard-projects-switch-function 'project-switch-project
   dashboard-startup-banner 'logo
   dashboard-items '((projects . 10) (xen-tip))
   dashboard-set-heading-icons t
   dashboard-set-file-icons t
   (append dashboard-item-generators) '(xen-tip . xen-dashboard-tip))
  (defun xen-dashboard-tip (list-size)
    "Insert a tip into the dashboard.

LIST-SIZE is ignored."
    (dashboard-insert-heading "Tip of the day" "t")
    (insert "\n")
    (let ((tips (with-temp-buffer
                  (insert-file-contents (locate-user-emacs-file "tips"))
                  (split-string (buffer-string) "\f" t))))
      (insert (elt tips (random (length tips)))))
    (dashboard-insert-shortcut 'tip "t" "Tip of the day"))

  (when (< (length command-line-args) 2)
    (dashboard-insert-startupify-lists)
    (add-hook 'elpaca-after-init-hook (lambda ()
                                        (switch-to-buffer dashboard-buffer-name)
                                        (goto-char (point-min))
                                        (redisplay)
                                        (run-hooks 'dashboard-after-initialize-hook)))))

;;; Navigation

;; Needed for avy binding.
(setup +global-override-map
  (:require +global-override-map))

(setup avy
  (:elpaca t)
  (:require +avy)
  (:option
   avy-background t
   avy-keys '(?u ?e ?o ?a ?h ?t ?n ?s)
   avy-style 'de-bruijn)
  (:global-override
   "S-SPC" +avy-goto-char-timer)
  (:global
   "M-g g" +avy-goto-line
   "M-g M-g" +avy-goto-line
   "M-u" avy-goto-char-in-line))

;; http://www.emacswiki.org/emacs/WindMove
;; Built in.
(setup windmove
  ;; Shift is default.
  (windmove-default-keybindings)
  (windmove-swap-states-default-keybindings '(shift meta))
  ;; Make windmove work in org-mode:
  (with-eval-after-load 'org
    (add-to-list 'org-shiftup-final-hook 'windmove-up)
    (add-to-list 'org-shiftleft-final-hook 'windmove-left)
    (add-to-list 'org-shiftdown-final-hook 'windmove-down)
    (add-to-list 'org-shiftright-final-hook 'windmove-right)))

;; http://www.emacswiki.org/emacs/FrameMove
(setup framemove
  ;; Elpaca doesn't have a menu for emacsmirror yet.
  (:elpaca :host github :repo "emacsmirror/framemove")
  (:require framemove)
  (setq framemove-hook-into-windmove t))

(setup mwim
  (:elpaca t)
  (:global
   "C-a" mwim-beginning
   "C-e" mwim-end))

;; http://www.emacswiki.org/emacs/WinnerMode
;; Built in.
(setup winner-mode
  (winner-mode))

;;; Editing

;; Standard Emacs package. Dead keys work when this is loaded.
(setup iso-transl
  (:require iso-transl))

;; Built in.
(setup delsel
  (delete-selection-mode))

(setup hungry-delete
  (:elpaca t)
  (:hook-into emacs-lisp-mode php-mode css-mode js-mode enh-ruby-mode crystal-mode))

(setup smartparens
  (:elpaca t)
  (:require smartparens)
  (:option
   ;; Let xen-paired-delete-mode handle deletion.
   sp-autodelete-closing-pair nil
   sp-autodelete-opening-pair nil
   sp-autodelete-pair nil
   ;; Only skip typed closing pair when we're at it.
   sp-autoskip-closing-pair 'always-end
   sp-show-pair-from-inside t)
  (:when-loaded
    (require 'smartparens-config)
    (smartparens-global-mode 1)
    (show-smartparens-global-mode 1)
    ;; Don't autopair ' when after a word (makes the first word of this
    ;; sentence difficult to type).
    (sp-pair "'" nil :unless '(sp-point-after-word-p))

    (sp-local-pair 'js-mode "/*" "*/" :actions '(wrap insert)
                   :post-handlers '(("* ||\n[i]" "RET") ("\n[i]* ||\n[i]" "*")))

    (sp-local-pair 'css-mode "/*" "*/" :actions '(wrap insert)
                   :post-handlers '(("* ||\n[i]" "RET")))

    (sp-local-pair 'twig-mode "{" nil :actions nil)
    (sp-local-pair 'twig-mode "{{" "}}" :actions '(wrap insert))
    (sp-local-pair 'twig-mode "{%" "%}" :actions '(wrap insert))
    (sp-local-pair 'twig-mode "{#" "#}" :actions '(wrap insert))
    ;; Hmm, no workie.
    ;; (with-eval-after-load "twig-mode"      (require 'smartparens-html))
    ;; (with-eval-after-load "smartparens" (sp-local-tag  'twig-mode "<" "<_>" "</_>" :transform 'sp-match-sgml-tags :post-handlers '(sp-html-post-handler)))
    ;; (require 'smartparens-html)
    ))

(setup xen-paired-delete
  (:load-from (concat user-emacs-directory "xen"))
  (with-eval-after-load 'smartparens
    (:require xen-paired-delete)
    (global-xen-paired-delete-mode)))

(setup expand-region
  (:elpaca t)
  (:option
   ;; Use subword expansion
   expand-region-subword-enabled t)
  (:global
   "C-S-SPC" er/expand-region))

(setup multiple-cursors
  (:elpaca t)
  (:global
   "C-<" mc/mark-previous-like-this
   "C->" mc/mark-next-like-this
   "C-M-m" mc/mark-more-like-this-extended
   "C-*" mc/mark-all-like-this
   "C-%" mc/mark-all-in-region
   "C-=" mc/mark-all-like-this-dwim))

(setup move-text
  (:elpaca t)
  (:global
   "C-M-<up>" move-text-up
   "C-M-<down>" move-text-down))

(setup multi-line
  (:elpaca t)
  (:global
   "C-c d" multi-line))

(setup ws-butler
  (:elpaca t)
  (:hook-into emacs-lisp-mode php-mode enh-ruby-mode css-mode js-mode feature-mode))

(setup undo-tree
  ;; Pull package directly from maintainer, the elpa package is behind.
  (:elpaca :type git :host gitlab :repo "tsc25/undo-tree")
  (:option
   undo-tree-auto-save-history t
   undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "undo-history")))
   undo-tree-visualizer-diff nil
   undo-tree-visualizer-relative-timestamps t
   undo-tree-visualizer-timestamps t
   ;; Don't use undo-tree in special-mode buffers
   undo-tree-incompatible-major-modes '(term-mode special-mode))
  (:with-map undo-tree-visualizer-mode-map
    (:bind
     ;; Make return accept currently selected revision and q
     ;; (and C-g) abort. The defaults are weird.
     "<return>" undo-tree-visualizer-quit
     "C-g" undo-tree-visualizer-abort
     "q" undo-tree-visualizer-abort))
  (global-undo-tree-mode))

(setup which-key
  (:elpaca t)
  (:option
   which-key-idle-secondary-delay 0.1
   which-key-mode t
   which-key-popup-type 'side-window
   which-key-show-early-on-C-h t
   which-key-side-window-max-height 0.5
   which-key-sort-order 'which-key-key-order)
  (which-key-mode))

(setup visual-fill-column
  (:elpaca t)
  (:option
   ;; Center text when using this mode
   visual-fill-column-center-text t))

(setup jinx
  (:elpaca t)
  (:when-loaded
    (:option
     ;; Add strings to faces spell-checked by jinx
     (append jinx-include-faces) '(php-mode font-lock-comment-face font-lock-string-face php-string))
    ;; Add PHP to camelCase modes
    (append jinx-camel-modes) 'php-mode)
  (:global
   "M-$" jinx-correct
   "C-M-$" jinx-languages)
  (global-jinx-mode)
  (with-eval-after-load 'vertico-multiform
    ;; Show jinx completions in a grid
    (add-to-list 'vertico-multiform-categories
                 '(jinx grid))))

(setup visual-regexp
  (:elpaca t)
  (:option
   ;; Show preview
   vr/default-replace-preview t)
  (:global
   "C-c r" vr/replace
   "C-c q" vr/query-replace
   "C-c m" vr/mc-mark))

(setup avy-zap
  (:elpaca t)
  (:global
   "M-Z" avy-zap-to-char-dwim
   "M-z" avy-zap-up-to-char-dwim))

(setup yasnippet
  (:elpaca t)
  (:require yasnippet)
  (:option
   yas-choose-keys-first nil
   yas-choose-tables-first t
   yas-fallback-behavior 'call-other-command
   yas-prompt-functions '(yas-dropdown-prompt yas-completing-prompt yas-ido-prompt yas-no-prompt)
   yas-triggers-in-field t
   yas-wrap-around-region nil)
  (:with-mode yas-minor-mode
    (:hook-into emacs-lisp-mode php-mode css-mode js-mode
                enh-ruby-mode git-commit-mode))
  (yas-reload-all))

;;; Files
;;;
;; Built in.
(setup savehist
  (:option
   ;; Save mini-buffer history
   savehist-save-minibuffer-history t
   ;; Other interesting things to save
   savehist-additional-variables '(kill-ring
                                   search-ring
                                   regexp-search-ring
                                   last-kbd-macro
                                   kmacro-ring
                                   shell-command-history
                                   ;; corfu-history
                                   ))
  (savehist-mode))

;; Built in, but we need to activate it.
(setup save-place
  (:option
   save-place-file (concat user-emacs-directory "saveplaces"))
  (save-place-mode))

;;; Completion

(setup vertico
  (:elpaca :files (:defaults "extensions/*"))
  (:option
   ;; Same keys as avy.
   vertico-quick1 "ueoa"
   vertico-quick2 "htns")
  (:with-map vertico-map
    (:bind
     "S-SPC" vertico-quick-exit))
  (vertico-mode)
  (vertico-multiform-mode 1))

(setup vertico-prescient
  (:elpaca t)
  (:option
   prescient-filter-method '(literal initialism prefix regexp)
   prescient-sort-full-matches-first t)
  (vertico-prescient-mode 1)
  (prescient-persist-mode 1))

(setup corfu
  (:elpaca :host github :repo "minad/corfu" :files (:defaults "extensions/*"))
  (:option
   ;; Enable automatic popup
   corfu-auto t
   ;; Trigger at one char
   corfu-auto-prefix 1
   ;; Show more candidates
   corfu-count 30
   ;; Let suggestions wrap around
   corfu-cycle t
   ;; Makes tab-and-go work better
   corfu-preselect 'prompt
   ;; Same quick keys as avy
   corfu-quick1 "ueoahtns"
   corfu-quick2 "ueoahtns")
  (:with-map corfu-map
    (:bind
     "TAB" corfu-next
     [tab]  corfu-next
     "S-TAB" corfu-previous
     [backtab] corfu-previous
     ;; Avy jumping.
     "S-SPC" corfu-quick-complete
     ;; Regain control over RET, C-n, C-p, <up>, and <down>.
     "RET" nil
     [remap next-line] nil
     [remap previous-line] nil
     "<down>" nil
     "<up>" nil))
  ;; Doesn't quite work?
  ;;(corfu-history-mode 1)
  (global-corfu-mode))

(setup consult
  (:elpaca t)
  ;; Not worth bothering to lazy-load, it's the first thing that gets
  ;; invoked anyway.
  (:require consult)
  (:also-load +consult)
  (:option
   ;; Limit the max fontification size to avoid sluggishness
   consult-fontify-max-size 102400
   consult-narrow-key "<"
   ;; Use Consult to select xref locations with preview.
   xref-show-xrefs-function #'consult-xref
   xref-show-definitions-function #'consult-xref)
  (:global
   "C-<tab>" +consult-line
   "C-x b" consult-buffer
   "M-y" consult-yank-pop
   "M-g m" consult-mark
   "M-g k" consult-global-mark)
  (:with-map isearch-mode-map
    (:bind
     "M-e" consult-isearch-history
     "C-<tab>" consult-line)))

(setup cape
  (:elpaca :host github :repo "minad/cape")
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(setup embark
  (:elpaca t)
  (:also-load +embark)
  (:option
   ;; Make embark-verbose-indicator shrink to content
   embark-verbose-indicator-display-action
   '(display-buffer-at-bottom (window-height . fit-window-to-buffer))
   ;; Use which-key to display options.
   embark-indicators '(+embark-which-key-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator)
   ;; Hide the mode line of the Embark live/completions buffers
   (append display-buffer-alist) '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                                   nil
                                   (window-parameters (mode-line-format . none))))
  (:global
   "C-." embark-act
   "C-," embark-dwim
   "C-h B" embark-bindings)
  ;; TODO: Move these to the packages that define the function.
  ;; (:embark region "s" sort-lines) would be nice.
  (:with-map embark-region-map
    (:bind
     "s" sort-lines
     "u" delete-duplicate-lines
     "/" +embark-google-region))
  ;; Consult previews in collect buffers.
  (:with-hook embark-collect-mode
    (:hook consult-preview-at-point-mode)))

(setup marginalia
  (:elpaca t)
  (:with-map minibuffer-local-map
    (:bind
     "M-A" marginalia-cycle))
  (marginalia-mode))

(setup orderless
  (:elpaca t)
  (:option completion-styles '(orderless partial-completion basic))
  (setq completion-category-defaults nil))

;;; Development

;;;; Emacs
(setup bug-hunter
  (:elpaca t))

(setup buttercup
  (:elpaca t))

(setup ecukes
  (:elpaca t))

;;;; General

(setup eglot
  (:require +eglot)
  (:with-function +eglot
    (:hook-into prog-mode)
    (with-eval-after-load 'yaml-mode
      (:hook-into yaml-mode)))
  ;; Don't pass Emacs process id to servers. Lang servers running in
  ;; docker can't see the Emacs process, so they think it died and
  ;; exits.
  (setq eglot-withhold-process-id t))

(setup eldoc
  (:option
   ;; Limit maximum number of lines displayed in the echo-area
   eldoc-echo-area-use-multiline-p 5)
  (:global
   "C-c ?" eldoc)
  (global-eldoc-mode 1))

(setup editorconfig
  (:elpaca t)
  (:option
   ;; Makefiles always use tabs. So exclude them.
   editorconfig-exclude-modes '(makefile-gmake-mode))
  (editorconfig-mode 1))

(setup flycheck
  (:elpaca t)
  (:option
   ;; PHP checkers are replaced by eglot and phpactor.
   flycheck-disabled-checkers '(javascript-jshint php php-phpcs)
   flycheck-global-modes (quote (not org-mode vterm-mode)))
  (:bind
   "M-<up>" flycheck-previous-error
   "M-<down>" flycheck-next-error)
  ;; Enable flycheck globally, doing it this way delays the setup to
  ;; after everything is loaded. TODO: :after-init macro.
  (:with-function global-flycheck-mode
    (:hook-into elpaca-after-init)))

(setup flycheck-eglot
  (:elpaca t)
  (with-eval-after-load 'eglot
    (with-eval-after-load 'flycheck
      (global-flycheck-eglot-mode 1))))

(setup consult-flycheck
  (:elpaca t)
  (with-eval-after-load 'flycheck
    ;; TODO: :bind-in, as :bind-into with a maps is deprecated, and
    ;; :bind is first effectuated when the feature is loaded.
    (define-key flycheck-command-map "!" 'consult-flycheck)))

(setup indentinator
  (:elpaca :host github :repo "xendk/indentinator")
  (:option
   ;; Speed up reindentation
   indentinator-idle-time 0.005)
  (:hook-into emacs-lisp-mode
              php-mode
              css-mode
              js-mode
              enh-ruby-mode
              twig-mode
              crystal-mode))

(setup magit
  (:elpaca t)
  (:also-load +magit)
  (:option
   ;; Don't bind global keys, we have our own
   magit-define-global-key-bindings nil
   ;; Full window status buffer
   magit-display-buffer-function (quote magit-display-buffer-fullframe-status-v1)
   ;; Prune branches when remote is removed
   magit-fetch-arguments (quote ("--prune"))
   ;; Save buffers when opening the status buffer
   magit-save-repository-buffers (quote dontask)
   ;; Don't require selecting a commit if point is already on one when
   ;; creating fixup and squash commits.
   magit-commit-squash-confirm nil)
  (:global
   "C-c g g" '("Status" . magit-status)
   "C-c g d" '("Dispatch" . magit-dispatch)
   "C-c g f" '("File dispatch" . magit-file-dispatch))
  (:with-hook git-commit-setup-hook
    (:hook +magit-commit-setup-jira))
  (:with-hook git-commit-mode-hook
    (:hook turn-on-auto-fill)
    ;; History in commit buffers
    (:hook git-commit-save-message)))

;; Try out https://github.com/doomelpa/code-review now that
;; github-review doesn't work anymore.
(setup forge
  (:elpaca t)
  (with-eval-after-load 'magit
    (require 'forge)))

(setup project
  (:require project)
  (:require +project)
  (:option
   ;; magit-extras normally sets this, but Magit is lazyloaded.
   (append project-switch-commands) '(magit-project-status "Magit")
   (append project-switch-commands) '(+project-vterm "vTerm" ?s)
   (append project-switch-commands) '(consult-ripgrep "Find regexp")
   ;; Remove those obsoleted by the above.
   (remove project-switch-commands) '(project-find-regexp "Find regexp")
   (remove project-switch-commands) '(project-eshell "Eshell")
   (remove project-switch-commands) '(project-vc-dir "VC-Dir")
   )
  (:with-map project-prefix-map
    (:bind
     "s" +project-switch-to-shell
     "S" +project-vterm
     ;; TODO: possibly move to new package
     "U" xen-docker-compose-up
     "g" consult-ripgrep)
    ;; Remove obsoleted.
    (:unbind "e" "v"))
  (with-eval-after-load 'magit
    (define-key project-prefix-map "m" 'magit-project-status))
  ;; Remap to the old projectile prefix. :global does not support
  ;; keymaps. Add :global-map?
  (keymap-global-set "C-c p" project-prefix-map))

(setup devdocs
  (:elpaca t)
  (:also-load +devdocs)
  (:global
   "C-c i" (lambda ()
             (interactive)
             (devdocs-lookup nil (thing-at-point 'symbol t))))
  ;; TODO: set these in the packages setup form?
  (:with-hook emacs-lisp-mode-hook
    (:local-set devdocs-current-docs '("elisp")))
  (:with-hook crystal-mode-hook
    (:local-set devdocs-current-docs '("crystal")))
  (:with-hook php-mode-hook
    (:local-set devdocs-current-docs '("php")))
  (:when-loaded
    (push '(a . +crystal-tag-a) (alist-get 'crystal devdocs-extra-rendering-functions))))

(setup cov
  (:elpaca t)
  (:face
   'cov-heavy-face '(:foreground "green")
   'cov-light-face '(:foreground "orange")
   'cov-none-face '(:foreground "red"))
  (:hook-into emacs-lisp-mode php-mode js-mode))

;;; File Modes

(setup apib-mode
  (:elpaca t)
  (:files "*.apib"))

(setup caddyfile-mode
  (:elpaca t)
  (:files "caddy.conf"))

(setup crystal-mode
  (:elpaca t)
  (:bind
   "C-c C-t" crystal-spec-switch)
  (:with-mode html-mode
    (:files "*.ecr"))
  ;; Tell eglot about the crystalline server.
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(crystal-mode . ("crystalline" "--stdio"))))
  (with-eval-after-load 'cape
    (defalias 'crystal-capf (cape-capf-super
                             (cape-capf-inside-code
                              (cape-capf-super #'cape-keyword #'cape-dabbrev))
                             ;; cape-dict could be handy, if we could
                             ;; get orderless to only prefix match it.
                             (cape-capf-inside-comment #'cape-dabbrev)))
    (:hook (lambda ()
             ;; Eglot sets up completion-at-point-functions in
             ;; its minor mode, so use eglot-managed-mode-hook
             ;; to override it.
             (add-hook 'eglot-managed-mode-hook
                       (lambda ()
                         (setq-local
                          completion-at-point-functions
                          (list #'crystal-capf)))
                       nil t)))))

(setup css-mode
  (:option
   ;; Use rainbow-mode hacked to use overlays so it works nicely with hl-line
   css-fontify-colors nil
   ;; Set default CSS indent offset
   css-indent-offset 2))

(setup dockerfile-mode
  (:elpaca t))

(setup drupal-mode
  (:elpaca :host github :repo "arnested/drupal-mode" :branch "develop")
  (:option
   drupal-ignore-paths-regexp "\\(vendor\\|node_modules\\|features/bootstrap\\|tests/behat\\|tests/codecept\\)"
   ;; Explicitly set this to nil to suppress trying to set flycheck-phpcs-standard
   drupal/phpcs-standard nil))

(setup enh-ruby-mode
  (:elpaca t)
  (:files "*.rb"))

(setup feature-mode
  (:elpaca t)
  ;; See readme for how to set up jump to step.
  )

(setup fish-mode
  (:elpaca t))

(setup go-mode
  (:elpaca t)
  (:when-loaded
    ;; Tell gopls that we're using go modules.
    (setenv "GO111MODULE" "on"))
  (:hook subword-mode))

(setup js-mode
  (:files "*.ts"))

(setup markdown-mode
  (:elpaca t)
  ;; Use gfm-mode in md files per default.
  (:with-function gfm-mode
    (:files ".md"))
  (:with-hook gfm-mode-hook
    (:hook auto-fill-mode)))

(setup nginx-mode
  (:elpaca t))

(setup org-mode
  (:files "*.org")
  (:option
   ;; Don't mess with using S-cursors for window selection
   org-support-shift-select t
   ;; Use cleaner looking org-indent-mode
   org-startup-indented t)
  ;; Might need a manual autoload for feature mode.
  )

(setup php-mode
  (:elpaca t)
  (:also-load +php-mode)
  (:files "*.module")
  (:option
   php-mode-coding-style 'psr2
   php-mode-enable-project-coding-style nil)
  (:bind
   ;; Override php-mode's binding of C-.
   "C-." embark-act
   "C-c u" +php-make-use)
  (:unbind
   ;; Unbind c-electric-paren to fall back to
   ;; self-insert-command, which allows smartparens to do
   ;; its magic.
   "(" ")")
  (:hook subword-mode)
  (:hook (lambda ()
           ;; Use -90 to make sure it gets in before
           ;; eglot-completion-at-point.
           (add-hook 'completion-at-point-functions
                     (cape-company-to-capf #'+php-mode-backend)
                     -90 t)))
  ;; TODO: New setup macro?
  (add-to-list 'magic-mode-alist
               '("<?php" . php-mode-maybe))
  (with-eval-after-load 'expand-region
    (:hook +php-mode-expansions))
  (with-eval-after-load 'smartparens
    (sp-with-modes '(php-mode)
      (sp-local-pair "/*" "*/" :post-handlers '(("| " "SPC")
                                                (" |\n[i]" "RET")
                                                (+php-handle-docstring "*")))

      ;; When pressing return as the first thing after inserting
      ;; a {, [ or (, add another and indent.
      (sp-local-pair "{" nil :post-handlers
                     '(("||\n[i]" "RET") +php-wrap-handler))
      (sp-local-pair "(" nil :post-handlers
                     '(("||\n[i]" "RET") +php-wrap-handler))
      (sp-local-pair "[" nil :post-handlers
                     '(("||\n[i]" "RET") +php-wrap-handler)))))

(setup po-mode
  (:elpaca t))

(setup rjsx-mode
  (:option
   ;; Strictly defined by js2-mode, but it's pulled in as a dependency.
   ;; Don't require semi-colons if not needed
   js2-strict-missing-semi-warning nil)
  (add-to-list 'magic-mode-alist
               '("import.*react" . rjsx-mode)))

(setup systemd
  (:elpaca t))

(setup twig-mode
  (:elpaca t))

(setup vcl-mode
  (:elpaca t)
  (:option
   vcl-indent-level 2))

(setup yaml-mode
  (:elpaca t)
  ;; Extend with neon and dist.
  (:file-match "\\.(e?ya?ml|neon)\\(.dist\\)$"))

;;; Tools

(setup custode
  (:elpaca :type git :host github :repo "xendk/custode.el")
  ;; Can't use :bind-keymap, as project-prefix-map doesn't exist until
  ;; project is loaded.
  (:when-loaded
    (with-eval-after-load 'project
      (define-key project-prefix-map "u" custode-prefix-map)))
  (global-custode-mode)
  ;; Add lighter to mode-line (this is how doom-modeline) suggests
  ;; adding a lighter for a single minor-mode.
  (add-to-list 'global-mode-string (list t custode-lighter)))

(setup reaper
  (:elpaca t)
  (:with-hook reaper-autofile-functions
    (:hook xen-reaper-autofile-function))
  (:global
   "C-c h" reaper)
  (load (locate-user-emacs-file "reaper-key.el") :noerror :nomessage)
  ;; Store the autofile function in an uncommitted file.
  (load (locate-user-emacs-file "xen-reaper.el") :noerror :nomessage))

(setup restclient
  (:elpaca t)
  (:files "*.http"))

(setup speed-type
  (:elpaca t))

(setup vterm
  (:elpaca t)
  (:require +vterm)
  (:option
   vterm-max-scrollback 100000
   vterm-buffer-name-string "vterm: %s"
   ;; Makes copy/paste work better
   vterm-copy-mode-remove-fake-newlines t
   ;; Same bold color handling as most terminals
   vterm-set-bold-hightbright t)
  ;; Would seem like a catch 22, but we always load consult.
  (with-eval-after-load 'consult
    (:global
     "C-c s" +vterm-switch-to-shell
     "C-c S" vterm))
  (:bind
   ;; Fish understands C-g too.
   "C-g" vterm--self-insert
   "C-q" vterm-send-next-key
   ;; Rebind M/C-cursors so they'll get sent to the process.
   "M-<up>" vterm--self-insert
   "M-<down>" vterm--self-insert
   "M-<right>" vterm--self-insert
   "M-<left>" vterm--self-insert
   "C-<up>" vterm--self-insert
   "C-<down>" vterm--self-insert
   "C-<right>" vterm--self-insert
   "C-<left>" vterm--self-insert
   "<delete>" vterm--self-insert
   "C-<backspace>" vterm--self-insert
   ;; Doesn't work anymore?
   "<mouse-4>" (lambda () (interactive) (vterm-send-key "<up>"))
   "<mouse-5>" (lambda () (interactive) (vterm-send-key "<down>"))
   ;; Make mouse buttons only select window, not move
   ;; point/start selection.
   "<mouse-1>" mouse-select-window
   "<down-mouse-1>" mouse-select-window
   "<mouse-3>" mouse-select-window
   "<down-mouse-3>" mouse-select-window
   ;; Let F11 be full screen, can't remember a shell
   ;; command where I use it.
   "<f11>" toggle-frame-fullscreen)
  (:with-map vterm-copy-mode-map
    (:bind
     ;; More ways to quit.
     "C-c C-c" vterm-copy-mode-done
     "C-d" vterm-copy-mode-done))
  ;; Disable string highlighting.
  (:hook (lambda ()
           ;; Don't fontify stings.
           (setq font-lock-defaults '('() t))))
  (:with-hook vterm-copy-mode-hook
    (:hook +vterm-copy-mode-hook)))

;;; Utils

(setup exec-path-from-shell
  (:elpaca t)
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(setup git-attr
  (:elpaca :host github :repo "arnested/emacs-git-attr"))

(setup google-this
  (:elpaca t)
  ;; TODO: maybe reimplement :bind-keymap as :autoload-keymap to lazy
  ;; load this.
  (google-this-mode))

(setup keyfreq
  (:elpaca t)
  (:option
   keyfreq-autosave-mode t
   keyfreq-file (concat user-emacs-directory "keyfreq")
   keyfreq-file-lock (concat user-emacs-directory "keyfreq.lock"))
  ;; Couldn't figure out :only-if
  (when xen-primary
    (keyfreq-mode 1)
    (keyfreq-autosave-mode 1)))

(setup s
  (:elpaca t))

(setup string-inflection
  (:elpaca t))

;;; My own stuff.
;; TODO: some of this should be moved to +files.
(setup xen
  (:load-from (concat user-emacs-directory "xen"))
  (:require xen)
  (:global
   "RET" xen-newline
   "M-SPC" xen-cycle-spacing
   "C-S-d" xen-duplicate-current-line
   "C-S-l" xen-mark-lines
   "C-c x" xen-map
   "M-c" xen-casing-map
   "C-c y" xen-edit-clipboard)
  (:with-map prog-mode-map
    (:bind
     "C-o" xen-open))
  (:with-map xen-casing-map
    (:bind
     "c" '("Capitalize" . capitalize-word)
     "u" '("UPPERCASE" . upcase-word)
     "l" '("downcase" . downcase-word)
     "s" '("snake_case" . string-inflection-underscore)
     "n" '("UPPER_SNAKE" . string-inflection-upcase)
     "a" '("camelCase" . string-inflection-lower-camelcase)
     "m" '("CamelCase" . string-inflection-camelcase)
     "k" '("kebab-case" . string-inflection-kebab-case)))
  ;; Tell delsel than xen-newline should delete selection.
  (put 'xen-newline 'delete-selection t)
  ;; Enable some disabled commands.
  (put 'narrow-to-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'scroll-left 'disabled nil))



(load custom-file)

;;; Some places for inspiration

;; https://github.com/tomjakubowski/.emacs.d/blob/master/init.el
;; http://www.aaronbedra.com/emacs.d/
;; http://batsov.com/articles/2011/11/30/the-ultimate-collection-of-emacs-resources/
;; https://www.youtube.com/watch?v=p3Te_a-AGqM&feature=player_embedded#!
;; http://emacsrocks.com/

(provide 'init)
;;; init.el ends here
