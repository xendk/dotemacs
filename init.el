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


;; Bit of hackery to update use-package.
(elpaca use-package
  (use-package use-package
    :config
    ;; Collect statistics for use-package-report.
    (setq use-package-compute-statistics t)
    :custom
    (use-package-verbose t)))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t)
  (setq use-package-compute-statistics t)
  (setq use-package-verbose t))

;; TODO: Figure out how to make a menu.
;; (defun elpaca-menu-xen (request_)
;;   "A minimal menu example.
;; Ignore REQUEST, as this is a static, curated list of packages."
;;   '((framemove :source "Xens stuff" :recipe (framemove :host github :repo "emacsmirror/framemove"))))

;; (elpaca-queue (elpaca nil (add-to-list 'elpaca-menu-functions #'elpaca-menu-xen)))

;; Block until current queue processed.
(elpaca-wait)

;; https://git.sr.ht/~pkal/emacs-init/tree/master/item/init.el
;; Also: https://github.com/progfolio/.emacs.d/blob/master/init.org
;; Use +notation.
;; Heavy: https://github.com/progfolio/.emacs.d/blob/master/init.org#custom-set-variables
;; Many good things: https://git.acdw.net/emacs/tree/?id=3e78d1f8ca5b100f39577790614433398bc6a422
;; Core emacs stuff. Some parts was nicked from https://github.com/grettke/lolsmacs
(setup emacs
  (:hide-mode auto-fill-function)
  (:hide-mode abbrev-mode)

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
   "S-C-<up>" enlarge-window
   ;; Alternatives: electric-buffer-list or bs-show.
   "C-x C-b" ibuffer)

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
  (:hide-mode)
  (:global
   "M-<left>" highlight-symbol-prev
   "M-<right>" highlight-symbol-next)
  (:hook-into emacs-lisp-mode php-mode css-mode js-mode enh-ruby-mode)

  (add-hook 'activate-mark-hook '+highlight-symbol-mode-deactivate)
  (add-hook 'deactivate-mark-hook '+highlight-symbol-mode-reactivate))

(setup page-break-lines
  (:elpaca t)
  (:hide-mode)
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
   avy-keys '(?a ?e ?u ?i ?d ?h ?t ?s)
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
  (:elpaca framemove :host github :repo "emacsmirror/framemove")
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
  (:hide-mode)
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

(setup ws-butler
  (:elpaca t)
  (:hide-mode)
  (:hook-into emacs-lisp-mode php-mode enh-ruby-mode css-mode js-mode feature-mode))

(setup undo-tree
  ;; Pull package directly from maintainer, the elpa package is behind.
  (:elpaca undo-tree :type git :host gitlab :repo "tsc25/undo-tree")
  (:hide-mode)
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
     (append jinx-include-faces) '(php-mode font-lock-comment-face font-lock-string-face php-string)))
  (:global
   "M-$" jinx-correct
   "C-M-$" jinx-languages)
  (global-jinx-mode))

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
  (:hide-mode yas-minor-mode)
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

;; Built in, used by magit.
(setup auto-revert
  (:hide-mode))

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

;;; Packages.

;; Reinstall these when the need arise:
;; cask-mode
;; git-link
;; geben
;; go-mode
;; list-processes+
;; multi-line

(use-package all-the-icons
  :config
  (add-to-list 'all-the-icons-mode-icon-alist '(vterm-mode all-the-icons-octicon "terminal" :v-adjust 0.2 :height 1.0)))

;; Make sure that delight is available as soon as any package triggers it.
(use-package delight
  :commands delight)

(use-package apib-mode
  :defer t
  :mode "\\.apib$")

(use-package bug-hunter
  :commands (bug-hunter-init-file bug-hunter-file))

(use-package buttercup
  :defer t)

(use-package caddyfile-mode
  :mode (("Caddyfile\\'" . caddyfile-mode)
         ("caddy\\.conf\\'" . caddyfile-mode)))

(use-package cape
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  :elpaca (:host github :repo "minad/cape"))

(use-package cask-mode
  :defer t)

(use-package copilot
  :elpaca (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  ;; Disabled while experimenting with eglot.
  :disabled
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-mode-map
              ("C-f" . copilot-accept-completion)
              ("C-S-f" . copilot-accept-completion-by-line)
              ("M-f" . copilot-accept-completion-by-word)
              ;; ("C-<tab>" . copilot-next-completion)
              ;; ("C-S-<tab>" . copilot-previous-completion)
              ;; ("C-g" . copilot-clear-overlay)
              ))

(use-package consult
  ;; xen-vterm requires this, and it's pretty much the first thing to
  ;; be triggered anyway.
  :demand t
  ;; Many more examples at https://github.com/minad/consult#use-package-example
  :bind (("C-<tab>" . xen-consult-line)
         ("C-x b" . consult-buffer)
         ("M-y" . consult-yank-pop)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("C-<tab>" . consult-line))
  :custom
  (consult-fontify-max-size 102400 "Limit the max fontification size to avoid sluggishness")
  :init
  ;; Use Consult to select xref locations with preview.
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (defun xen-consult-line ()
    "Call consult-line, using region as inital input, if active."
    (interactive)
    (if (use-region-p)
        (progn
          (deactivate-mark)
          (consult-line (buffer-substring-no-properties (region-beginning) (region-end))))
      (consult-line)))
  (defun xen-consult-ripgrep ()
    "Call consult-repgrep, using region as inital input, if active."
    (interactive)
    (if (use-region-p)
        (progn
          (deactivate-mark)
          (consult-ripgrep nil (buffer-substring-no-properties (region-beginning) (region-end))))
      (consult-ripgrep)))
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Use project.el for getting project root.
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project))))))

(use-package consult-flycheck
  :commands consult-flycheck
  :after (flycheck)
  :bind (:map flycheck-command-map
              ("!" . consult-flycheck)))

(use-package corfu
  ;; TAB-and-Go customizations
  :custom
  (corfu-auto t "Enable automatic popup")
  (corfu-auto-prefix 1 "Trigger at one char")
  (corfu-count 30 "Show more candidates")
  (corfu-cycle t "Let suggestions wrap around")
  (corfu-preselect 'prompt "Makes tab-and-go work better")
  ;; Use TAB for cycling, default is `corfu-complete'.
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)
        ;; Avy jumping.
        ("S-SPC" . corfu-quick-complete)
        ;; Regain control over RET, C-n, C-p, <up>, and <down>.
        ("RET" . nil)
        ([remap next-line] . nil)
        ([remap previous-line] . nil)
        ("<down>" . nil)
        ("<up>" . nil))
  :init
  (global-corfu-mode)
  ;; Doesn't quite work?
  ;;(corfu-history-mode 1)
  ;; Clone from GitHup rather than ELPA.
  :elpaca (:host github :repo "minad/corfu" :files (:defaults "extensions/*")))

(use-package cov
  :custom-face
  (cov-heavy-face ((t (:foreground "green"))))
  (cov-light-face ((t (:foreground "orange"))))
  (cov-none-face ((t (:foreground "red"))))
  :hook
  ((emacs-lisp-mode php-mode js-mode) . cov-mode))

(use-package crystal-mode
  ;; ECR files can be anything really, but html-mode is what I most
  ;; often use.
  :mode ("\\.ecr\\'" . html-mode)
  :init
  ;; Tell eglot about the crystalline server.
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(crystal-mode . ("crystalline" "--stdio"))))
  ;; Crystalines completion is non-existent, so use keyword and
  ;; dabbrev completion from cape instead.
  (defalias 'crystal-capf (cape-capf-super
                           (cape-capf-inside-code
                            (cape-capf-super #'cape-keyword #'cape-dabbrev))
                           ;; cape-dict could be handy, if we could
                           ;; get orderless to only prefix match it.
                           (cape-capf-inside-comment #'cape-dabbrev)))
  :hook
  (crystal-mode . (lambda ()
                    ;; Eglot sets up completion-at-point-functions in
                    ;; its minor mode, so use eglot-managed-mode-hook
                    ;; to override it.
                    (add-hook 'eglot-managed-mode-hook
                              (lambda ()
                                (setq-local
                                 completion-at-point-functions
                                 (list #'crystal-capf)))
                              nil t)))
  :bind (:map crystal-mode-map
              ("C-c C-t" . crystal-spec-switch)))

(use-package css-mode
  :elpaca nil
  :custom
  (css-fontify-colors nil "Use rainbow-mode hacked to use overlays so it works nicely with hl-line")
  (css-indent-offset 2 "Set default CSS indent offset")
  :commands css-mode)

(use-package custode
  :load-path "custode"
  :elpaca (:type git :host github :repo "xendk/custode.el")
  :init
  (global-custode-mode)
  :config
  ;; Can't use :bind-keymap, as project-prefix-map doesn't exist until
  ;; project is loaded.
  (with-eval-after-load "project"
    (define-key project-prefix-map "u" custode-prefix-map))
  ;; Add lighter to mode-line (this is how doom-modeline) suggests
  ;; adding a lighter for a single minor-mode.
  (add-to-list 'global-mode-string (list t custode-lighter)))

(use-package dap-mode
  :commands (dap-mode dap-ui-mode)
  :custom
  (dap-php-debug-program
   (quote
    ("node" (concat user-emacs-directory "vscode-php-debug/out/phpDebug.js")))))

;; To use dap, the container running xdebug needs something like:
;; export XDEBUG_CONFIG="remote_host=172.17.0.1 remote_connect_back=Off remote_autostart=On"
;; And dap needs something like this:
;; (dap-register-debug-template "My Php Debug"
;;   (list :type "php"
;;         :cwd nil
;;         :request "launch"
;;         :name "Php Debug"
;;         :args '("--server=4711")
;;         :stopOnEntry t
;;         :pathMappings (ht ("/var/www/web/" "/home/xen/sites/ding2/web/"))
;;         :sourceMaps t))

(use-package devdocs
  :bind (("C-c i" . (lambda ()
                      (interactive)
                      (devdocs-lookup nil (thing-at-point 'symbol t)))))
  :hook
  (emacs-lisp-mode . (lambda ()
                       (setq-local devdocs-current-docs '("elisp"))))
  (crystal-mode . (lambda ()
                    (setq-local devdocs-current-docs '("crystal"))))
  (php-mode . (lambda ()
                (setq-local devdocs-current-docs '("php")))))

(use-package dockerfile-mode
  :defer t)

(use-package drupal-mode
  :defer t
  :custom
  (drupal-ignore-paths-regexp
   "\\(vendor\\|node_modules\\|features/bootstrap\\|tests/behat\\|tests/codecept\\)")
  (drupal/phpcs-standard nil "Explicitly set this to nil to suppress trying to set flycheck-phpcs-standard")
  :delight drupal-mode '(:eval (list " " (propertize (concat [#xF1A9])
                                                     'face '(:family "FontAwesome"))))
  :elpaca (:host github :repo "arnested/drupal-mode" :branch "develop"))

;; Part of drupal-mode.
(use-package drush-make-mode
  :elpaca nil
  :after drupal-mode)

(use-package ecukes
  :commands ecukes)

(use-package editorconfig
  :custom
  ;; Makefiles always use tabs. So exclude them.
  (editorconfig-exclude-modes '(makefile-gmake-mode))
  :config
  (editorconfig-mode 1))

;; For editing code blocks in Markdown mode.
(use-package edit-indirect
  :after markdown-mode)

(use-package eglot
  :elpaca nil
  :hook
  ((prog-mode yaml-mode) . (lambda ()
                             (when (eglot--lookup-mode major-mode)
                               (eglot-ensure))))
  :config
  ;; Don't pass Emacs process id to servers. Lang servers running in
  ;; docker can't see the Emacs process, so they think it died and
  ;; exits.
  (setq eglot-withhold-process-id t))

;; TODO: Use global-eldoc-mode instead of enabling in prog-mode?
(use-package eldoc
  :commands (eldoc-mode eldoc)
  :custom
  (eldoc-echo-area-use-multiline-p 5 "Limit maximum number of lines displayed in the echo-area")
  :bind
  ("C-c ?" . eldoc)
  :delight)

(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-," . embark-dwim)
   ("C-h B" . embark-bindings)
   :map embark-region-map
   ("s" . sort-lines)
   ("u" . delete-duplicate-lines)
   ("/" . xen-google-region))
  :custom
  (embark-verbose-indicator-display-action
   '(display-buffer-at-bottom (window-height . fit-window-to-buffer))
   "Make embark-verbose-indicator shrink to content")
  :config
  ;; Use which-key as indicator, rather than the minimal default or
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-argument" (cdr binding))))))))

  (setq embark-indicators
        '(embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator)  ;; the completing-read one.
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  (defun xen-google-region ()
    "Google current region."
    (interactive)
    (google-this-region nil t)))

(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package enh-ruby-mode
  :mode "\\.rb\\'")

(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package feature-mode
  ;; See readme for how to set up jump to step.
  :defer t)

(use-package fish-mode
  :defer t)

(use-package flycheck
  :custom
  ;; PHP checkers are replaced by eglot and phpactor.
  (flycheck-disabled-checkers '(javascript-jshint php php-phpcs)
                              "Disable unwanted checkers")
  (flycheck-eslintrc nil)
  (flycheck-global-modes (quote (not org-mode vterm-mode)))
  ;; this has long been overwritten by doom-modelines `check' segment.
  (flycheck-mode-line (quote (:eval (xen-flycheck-mode-line-status-text))))
  (flycheck-scss-compass t)
  :bind (:map flycheck-mode-map
              ("M-<up>" . flycheck-previous-error)
              ("M-<down>" . flycheck-next-error))
  ;; Enable flycheck globally, doing it this way delays the setup to
  ;; after everything is loaded.
  :hook (elpaca-after-init . global-flycheck-mode))

(use-package flycheck-cask
  :commands flycheck-cask-setup
  :hook (flycheck-mode . flycheck-cask-setup))

(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

(use-package flycheck-package
  :commands flycheck-package-setup
  :hook (flycheck-mode . flycheck-package-setup))

(use-package forge
  :after magit)

(use-package git-attr
  :elpaca (:host github :repo "arnested/emacs-git-attr"))

(use-package go-mode
  :mode "\\.go\\'"
  :defer t
  :config
  ;; Tell gopls that we're using go modules.
  (setenv "GO111MODULE" "on")
  :hook ((go-mode . subword-mode)))

(use-package google-this
  :commands (google-this-mode google-this-region)
  :delight
  ;; Why does this use use-package-autoload-keymap?
  :bind-keymap ("C-c /" . google-this-mode-submap)
  :config (google-this-mode))

(use-package github-review
  :after forge)

(use-package ibuffer-vc
  :hook
  (ibuffer . (lambda ()
               (ibuffer-vc-set-filter-groups-by-vc-root)
               (unless (eq ibuffer-sorting-mode 'alphabetic)
                 (ibuffer-do-sort-by-alphabetic)))))

(use-package indentinator
  :hook ((emacs-lisp-mode
          cask-mode
          php-mode
          css-mode
          js-mode
          enh-ruby-mode
          twig-mode
          crystal-mode) . indentinator-mode)
  :custom
  (indentinator-idle-time 0.005 "Speed up indentinator")
  :elpaca (:host github :repo "xendk/indentinator"))

;; Properly handle annotations in java-mode.
(use-package java-mode-indent-annotations
  :elpaca nil
  :load-path "lib/"
  :commands java-mode-indent-annotations-setup
  :hook (java-mode . java-mode-indent-annotations-setup))

;; Built in.
(use-package js-mode
  :elpaca nil
  :commands js-mode
  :mode "\\.ts$")

(use-package keyfreq
  :if xen-primary
  :commands (keyfreq-mode keyfreq-autosave-mode)
  :custom
  (keyfreq-autosave-mode t)
  (keyfreq-file (concat user-emacs-directory "keyfreq"))
  (keyfreq-file-lock (concat user-emacs-directory "keyfreq.lock"))
  :init
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package literate-calc-mode
  :commands literate-calc-mode
  :defer t)

(use-package nginx-mode
  :commands nginx-mode)

(use-package magit
  :defines magit-last-seen-setup-instructions
  :custom
  ;; https://git.sr.ht/~pkal/emacs-init/tree/master/item/init.el#L288
  (magit-define-global-key-bindings nil "Don't bind global keys, we have our own")
  (magit-display-buffer-function (quote magit-display-buffer-fullframe-status-v1))
  (magit-fetch-arguments (quote ("--prune")))
  (magit-push-always-verify nil)
  (magit-revert-buffers t t)
  (magit-save-repository-buffers (quote dontask))
  (magit-status-buffer-switch-function (quote switch-to-buffer))
  (magit-use-sticky-arguments (quote current))
  ;; Don't require selecting a commit if point is already on one when
  ;; creating fixup and squash commits.
  (magit-commit-squash-confirm nil)
  :init (setq magit-last-seen-setup-instructions "1.4.0")
  ;; After upgrading to 29, this doesn't work anymore.
  :bind (("C-c g g" ("Status" . magit-status))
         ("C-c g d" ("Dispatch" . magit-dispatch))
         ("C-c g f" ("File dispatch" . magit-file-dispatch)))
  :hook
  (git-commit-setup . xen-git-commit-setup)
  (git-commit-mode . turn-on-auto-fill)
  (git-commit-mode . git-commit-save-message)
  :config
  (delight 'magit-status-mode
           (propertize (concat " " [#xF1D3])
                       'face '(:family "FontAwesome")) :major))

;; Why this isn't handled by dependencies, I don't know.
(use-package magit-section)

(use-package magit-filenotify
  :defer t)

(use-package markdown-mode
  :mode
  ("\\.\\(m\\(ark\\)?down\\)$" . markdown-mode)
  ("\\.md$" . gfm-mode)
  :hook (gfm-mode . auto-fill-mode))

(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (;("M-A" . marginalia-cycle)
         :map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package multi-line
  :bind
  ("C-c d" . multi-line))

;; From http://www.gerd-neugebauer.de/software/emacs/multi-mode/multi-mode.el
(use-package multi-mode
  :load-path "lib/"
  :commands multi-mode)
;; Example:
;; (multi-mode 1 'html-mode '("<?php" php-mode) '("?>" html-mode))

(use-package olivetti
  :defer t)

(use-package orderless
  :init
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        completion-category-overrides nil))

;; We're using the built in version of org. Upgrading it requires some hackery:
;; https://github.com/raxod502/radian/blob/ee92ea6cb0473bf7d20c6d381753011312ef4a52/radian-emacs/radian-org.el#L46-L112
;; And as we're quite content with it, we're sticking with the built in version.
(use-package org-mode
  :elpaca nil
  :mode "\\.org\\'"
  :custom
  (org-support-shift-select t "Don't mess with using S-cursors for window selection")
  (org-startup-indented t "Use cleaner looking org-indent-mode"))
;; And we have to define it twice. Using the name `org' means that the
;; :mode in the above can't load the file, but using `org-mode' means
;; that `turn-on-orgtbl' can't load the file.
(use-package org
  :elpaca nil
  ;; orgtbl is used by feature-mode.
  :commands (turn-on-orgtbl))

;; package-lint requires package for its package database. So we defer
;; it and use :config to initialize it when someone requires it.
(use-package package
  :elpaca nil
  :commands package-initialize
  :defer t
  :config
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize))

(use-package php-boris
  :commands php-boris)

(use-package php-mode
  :commands php-mode
  :mode
  ("\\.php\\'" . php-mode)
  ("\\.inc\\'" . php-mode)
  ("\\.module\\'" . php-mode)
  :magic ("<?php" . php-mode-maybe)
  :init
  ;; A custom company-backend (which cape-company-to-capf will make a
  ;; proper capf) completes some very common PHP idioms.
  (defvar xen-php-mode-backend-alist
    '(("declare(strict_types=1);" . "declare")
      ("<?php" . "<?ph")
      (xen-php-mode-backend-prefix . "class ")
      (xen-php-mode-backend-prefix . "interface ")
      (xen-php-mode-backend-prefix . "trait ")))
  (defun xen-php-mode-backend-prefix (prefix)
    "Return class/interface based on the current file name."
    (concat prefix (file-name-sans-extension
                    (file-name-nondirectory (buffer-file-name)))))
  (defun xen-php-mode-backend (action &optional arg &rest _)
    (pcase action
      ('prefix (let ((prefix (save-excursion
                               (let ((end (point)))
                                 (beginning-of-line)
                                 (buffer-substring (point) end)))))
                 (when (cl-some (lambda (cand)
                                  (string-prefix-p prefix (cdr cand)))
                                xen-php-mode-backend-alist)
                   (cons prefix t))))
      ('candidates (all-completions
                    arg
                    (mapcar
                     (lambda (cand)
                       (if (functionp (car cand))
                           (cons (funcall (car cand) (cdr cand)) (cdr cand))
                         cand))
                     xen-php-mode-backend-alist)))))
  :bind (:map php-mode-map
              ;; Override php-mode's binding of C-.
              ("C-." . embark-act))
  :custom
  (php-mode-coding-style (quote psr2))
  (php-mode-enable-project-coding-style nil)
  :config
  (require 'dap-php)
  :hook (php-mode . (lambda () (subword-mode 1)))
  (php-mode . (lambda ()
                ;; Use -90 to make sure it gets in before
                ;; eglot-completion-at-point.
                (add-hook 'completion-at-point-functions
                          (cape-company-to-capf #'xen-php-mode-backend)
                          -90 t))))

(use-package po-mode
  :defer t)

(use-package project
  ;; Remap to the old projectile prefix.
  :bind-keymap ("C-c p" . project-prefix-map)
  :config
  ;; Use consult-ripgrep instead of project-find-regexp.
  ;;(advice-add #'project-find-regexp :override #'consult-ripgrep)
  ;; magit-extras normally sets this, but Magit is lazyloaded.
  (add-to-list 'project-switch-commands '(magit-project-status "Magit") t)
  :bind (:map project-prefix-map
              ("m" . magit-project-status))
  :elpaca nil)

(use-package reaper
  :config
  (load (locate-user-emacs-file "reaper-key.el") :noerror :nomessage)
  ;; Store the autofile function in an uncommitted file.
  (load (locate-user-emacs-file "xen-reaper.el") :noerror :nomessage)
  :init
  (add-hook 'reaper-autofile-functions 'xen-reaper-autofile-function)
  :bind ("C-c h" . reaper))

(use-package restclient
  :defer t
  :mode (("\\.http$" . restclient-mode)))

(use-package rjsx-mode
  :commands rjsx-mode
  :mode "components\\/.*\\.js\\'"
  :magic ("import.*react" . rjsx-mode)
  :custom
  ;; Strictly defined by js2-mode, but it's pulled in as a dependency.
  (js2-strict-missing-semi-warning nil "Don't require semi-colons if not needed"))

(use-package s
  :commands s-truncate)

(use-package smex
  :custom
  (smex-save-file (concat user-emacs-directory "smex-items"))
  ;; autoload when needed.
  :defer t)

(use-package speed-type
  :commands (speed-type-text speed-type-region speed-type-buffer))

;; Suggested by forge.
(use-package sqlite3)

(use-package string-inflection
  ;; autoload when needed.
  :commands (string-inflection-underscore
             string-inflection-upcase
             string-inflection-lower-camelcase
             string-inflection-camelcase
             string-inflection-kebab-case))

(use-package systemd
  :defer t)

;; (use-package tempel
;;   :bind (:map tempel-map
;;               ("<tab>" . tempel-next)))
;;
(use-package twig-mode
  :defer t)

(use-package vcl-mode
  :commands vcl-mode
  :mode "\\.vcl\\'"
  :custom
  (vcl-indent-level 2 "Set indent level"))

(use-package vertico
  :elpaca (:files (:defaults "extensions/*"))
  :bind (:map vertico-map
              ("S-SPC" . vertico-quick-exit))
  :custom
  (vertico-quick1 "aoeu" "Set quick navigation keys")
  (vertico-quick2 "dhtn" "Ditto")
  :init
  (vertico-mode))

(use-package vertico-multiform
  :after (vertico jinx)
  ;; Part of vertico.
  :elpaca nil
  :custom
  (jinx-camel-modes '(java-mode
                      java-ts-mode
                      js-mode
                      js-ts-mode
                      ruby-mode
                      ruby-ts-mode
                      rust-mode
                      rust-ts-mode
                      haskell-mode
                      kotlin-mode
                      swift-mode
                      csharp-mode
                      csharp-ts-mode
                      objc-mode
                      typescript-ts-mode
                      typescript-mode
                      python-mode
                      python-ts-mode
                      dart-mode
                      go-mode
                      go-ts-mode
                      scala-mode
                      groovy-mode
                      php-mode) "Add PHP to camelCase modes")
  :config
  ;; Show jinx completions in a grid.
  (add-to-list 'vertico-multiform-categories
               '(jinx grid))
  (vertico-multiform-mode 1))

(use-package vertico-prescient
  :init
  ;; todo: is this customs?
  (setq prescient-filter-method '(literal initialism prefix regexp)
        prescient-sort-full-matches-first t)
  (vertico-prescient-mode 1)
  (prescient-persist-mode 1))

(use-package vterm
  :custom
  (vterm-max-scrollback 100000)
  (vterm-buffer-name-string "vterm: %s")
  (vterm-copy-mode-remove-fake-newlines t "Makes copy/paste work better")
  (vterm-set-bold-hightbright t "Same bold color handling as most terminals")
  :bind (:map vterm-mode-map
              ;; Fish understands C-g too.
              ("C-g" . vterm--self-insert)
              ("C-q" . vterm-send-next-key)
              ;; Rebind M/C-cursors so they'll get sent to the process.
              ("M-<up>" . vterm--self-insert)
              ("M-<down>" . vterm--self-insert)
              ("M-<right>" . vterm--self-insert)
              ("M-<left>" . vterm--self-insert)
              ("C-<up>" . vterm--self-insert)
              ("C-<down>" . vterm--self-insert)
              ("C-<right>" . vterm--self-insert)
              ("C-<left>" . vterm--self-insert)
              ("<delete>" . vterm--self-insert)
              ("C-<backspace>" . vterm--self-insert)
              ("<mouse-4>" . (lambda () (interactive) (vterm-send-key "<up>")))
              ("<mouse-5>" . (lambda () (interactive) (vterm-send-key "<down>")))
              ;; Make mouse buttons only select window, not move
              ;; point/start selection.
              ("<mouse-1>" . mouse-select-window)
              ("<down-mouse-1>" . mouse-select-window)
              ("<mouse-3>" . mouse-select-window)
              ("<down-mouse-3>" . mouse-select-window)
              ;; Let F11 be full screen, can't remember a shell
              ;; command where I use it.
              ("<f11>" . toggle-frame-fullscreen)
              :map vterm-copy-mode-map
              ;; More ways to quit.
              ("C-c C-c" . vterm-copy-mode-done)
              ("C-d" . vterm-copy-mode-done))
  :hook
  ;; Disable string highlighting.
  (vterm-mode . (lambda ()
                  ;; Don't fontify stings.
                  (setq font-lock-defaults '('() t)))))

;; Writable grep buffer.
;; (use-package wgrep
;;   )

(use-package xen
  :elpaca nil
  :load-path "xen"
  :demand t
  :config
  ;; Tell delsel than xen-newline should delete selection.
  (put 'xen-newline 'delete-selection t)
  :bind (("RET" . xen-newline)
         ("M-SPC" . xen-cycle-spacing)
         ;; ("M-l" . xen-avy-goto-line)
         ("<f12>" . olivetti-mode)
         ("C-S-d" . xen-duplicate-current-line)
         ("C-S-l" . xen-mark-lines)
         ("C-c x" . xen-map)
         ("M-c" . xen-casing-map)
         ("C-c y" . xen-edit-clipboard)
         :map prog-mode-map
         ("C-o" . 'xen-open)
         :map xen-casing-map
         ("c" ("Capitalize" . capitalize-word))
         ("u" ("UPPERCASE" . upcase-word))
         ("l" ("downcase" . downcase-word))
         ("s" ("snake_case" . string-inflection-underscore))
         ("n" ("UPPER_SNAKE" . string-inflection-upcase))
         ("a" ("camelCase" . string-inflection-lower-camelcase))
         ("m" ("CamelCase" . string-inflection-camelcase))
         ("k" ("kebab-case" . string-inflection-kebab-case))))

(use-package xen-flycheck
  :elpaca nil
  :load-path "xen"
  :functions xen-flycheck-mode-line-status-text
  :bind (:map flycheck-mode-map
              ("C-c ! s" . xen-flycheck-insert-suppressor)))

(use-package xen-php
  :elpaca nil
  :load-path "xen"
  :demand
  ;; Use hack-local-variables-hook to run after `.dir-local.el'
  ;; variables has been set.
  :config
  (with-eval-after-load "expand-region"
    (add-hook 'php-mode-hook #'xen-php-mode-expansions))
  (sp-with-modes '(php-mode)
    (sp-local-pair "/*" "*/" :post-handlers '(("| " "SPC")
                                              (" |\n[i]" "RET")
                                              (xen-php-handle-docstring "*")))

    ;; When pressing return as the first thing after inserting
    ;; a {, [ or (, add another and indent.
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET") xen-php-wrap-handler))
    (sp-local-pair "(" nil :post-handlers '(("||\n[i]" "RET") xen-php-wrap-handler))
    (sp-local-pair "[" nil :post-handlers '(("||\n[i]" "RET") xen-php-wrap-handler)))
  :bind (:map php-mode-map
              ;; Unbind c-electric-paren ta fall back to
              ;; self-insert-command, which allows smartparens to do
              ;; its magic.
              ("(" . nil)
              (")" . nil)
              ("C-c u" . xen-php-make-use))
  :after (php-mode smartparens))

(use-package xen-project
  :elpaca nil
  :load-path "xen"
  :after (project)
  :bind (:map project-prefix-map
              ("s" . xen-project-switch-to-shell)
              ("S" . xen-project-vterm)
              ("U" . xen-docker-compose-up)
              ("g" . consult-ripgrep)
              ;; Remove obsoleted.
              ("e" . nil)
              ("v" . nil))
  :init
  (add-to-list 'project-switch-commands '(xen-project-vterm "vTerm" ?s) t)
  (add-to-list 'project-switch-commands '(consult-ripgrep "Find regexp") t)
  ;; Remove those obsoleted by the above.
  (setq project-switch-commands (delete '(project-find-regexp "Find regexp") project-switch-commands))
  (setq project-switch-commands (delete '(project-eshell "Eshell") project-switch-commands))
  (setq project-switch-commands (delete '(project-vc-dir "VC-Dir") project-switch-commands))
  )

(use-package xen-vterm
  :elpaca nil
  :load-path "xen"
  :hook (vterm-copy-mode . xen-vterm-copy-mode-hook)
  :bind
  ("C-c s" . xen-switch-to-shell)
  ("C-c S" . vterm))

(use-package yaml-mode
  :mode "\\.(e?ya?ml|neon)\\(.dist\\)$")



(load custom-file)

;; Enable some disabled commands.
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)

;;; Some places for inspiration

;; https://github.com/tomjakubowski/.emacs.d/blob/master/init.el
;; http://www.aaronbedra.com/emacs.d/
;; http://batsov.com/articles/2011/11/30/the-ultimate-collection-of-emacs-resources/
;; https://www.youtube.com/watch?v=p3Te_a-AGqM&feature=player_embedded#!
;; http://emacsrocks.com/

(provide 'init)
;;; init.el ends here
