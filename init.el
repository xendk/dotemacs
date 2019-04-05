;;; init.el --- Xens emacs configuration.  -*- flycheck-emacs-lisp-load-path: inherit; -*-
;;; Commentary:

;; My Emacs init.el.

;;; Code:
;; Set foreground and background color to avoid flashing when
;; doom-theme gets activated.
(set-face-foreground 'default "#bbc2cf")
(set-face-background 'default "#21242b")

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

;;; Configuration.
;; Relocate and load customs (so we don't clutter init.el with them).
;; Loading them first so colors, faces and menu/toolbar/scrollbar is
;; removed early.
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file)

;;; Aliases and advices
;; I'm grown up, I can manage using y/n for even destructive commands.
(defalias 'yes-or-no-p 'y-or-n-p)



;; Initialize straight package system.
(let ((bootstrap-file (concat user-emacs-directory "straight/bootstrap.el"))
      (bootstrap-version 2))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Ensure that flycheck can see that straight will be available for
;; the straight-register-package.
(eval-and-compile
  (require 'straight))
;; Work around seq package disappearing from EmacsMirror. See
;; https://github.com/raxod502/straight.el/issues/170
(straight-register-package
 '(seq :repo "https://git.savannah.gnu.org/git/emacs/elpa.git" :files ("packages/seq/*.el")))

;; Bootstrap `use-package'
(straight-use-package 'use-package)
;; When flycheck checks this file it needs use-package (and straight
;; above) loaded to understand the use-package forms. Also the reason
;; for the flycheck-emacs-lisp-load-path on the first line of the
;; file.
(eval-and-compile (require 'use-package))

;;; Bindings.
;; Used to M-DEL deleting a word.
(bind-key "M-<delete>" 'kill-word)

;; Don't iconify on C-z.
(unbind-key "C-z")

;; Scrolling on C-v confuses me when my muscle memory tries to use it as paste.
(unbind-key "C-v")
;; Take out it's mate for consistency.
(unbind-key "M-v")

;; I'm fat-fingering this when changing buffers some times. Having to
;; delete the window is annoying.
(unbind-key "C-x C-b")

;; Horizontal scrolling on trackpad produces these, which makes Emacs
;; print warnings about undefined keys. I don't want to do anything on
;; horizontal scroll.
(global-set-key (kbd "<mouse-6>") 'ignore)
(global-set-key (kbd "<mouse-7>") 'ignore)

;; Window resizing.
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; Quickly open URLs.
(bind-key "C-c b" 'browse-url-at-point)



;;; Packages.

;; Reinstall these when the need arise:
;; buttercup
;; cask-mode
;; git-link
;; geben
;; go-mode
;; list-processes+
;; multi-line

;; Load doom-themes and doom-modeline first so we wont end up in
;; default colors in case of error later.
(use-package doom-themes
  :config
  ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
  ;; may have their own settings.
  (load-theme 'doom-one t)

  ;; Set font.
  (set-face-attribute 'default nil :height 110 :width
                      'semi-condensed :foundry "FBI " :family "Input")
  ;; Make comments starker colors.
  (set-face-foreground 'font-lock-comment-face (doom-lighten 'cyan .5))
  (set-face-foreground 'font-lock-doc-face (doom-lighten 'cyan .25))

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  :straight t)

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-buffer-file-name-style 'truncate-except-project)
  :config
  ;; Define a custom mode-line segment.
  (doom-modeline-def-segment buffer-info-simple-icon
    "As `buffer-info', but without state icon."
    (let ((active (doom-modeline--active)))
      (concat
       " "

       ;; major mode icon
       (when (and doom-modeline-icon doom-modeline-major-mode-icon)
         (when-let ((icon (or doom-modeline--buffer-file-icon
                              (doom-modeline-update-buffer-file-icon))))
           (concat
            (if (and active doom-modeline-major-mode-color-icon)
                icon
              (propertize icon
                          'face `(:height
                                  ,(doom-modeline-icon-height 1.1)
                                  :family
                                  ,(all-the-icons-icon-family icon)
                                  :inherit)))
            doom-modeline-vspc)))

       ;; buffer file name
       (when-let ((name (or doom-modeline--buffer-file-name
                            (doom-modeline-update-buffer-file-name))))
         (if active
             name
           (propertize name 'face 'mode-line-inactive))))))
  ;; As 'minimal, but without buffer state icon.
  (doom-modeline-def-modeline 'xen-minimal
    '(bar matches " " buffer-info-simple-icon)
    '(media-info major-mode " "))
  ;; Use 'xen-minimal in term buffes.
  :hook (term-mode . (lambda ()
                       (doom-modeline-set-modeline 'xen-minimal)))
  :straight t)

(use-package all-the-icons
  :straight t)

;; Make sure that delight is available as soon as any package triggers it.
(use-package delight
  :commands delight
  :straight t)

(use-package apib-mode
  :defer t
  :mode "\\.apib$"
  :straight t)

(use-package ag
  :defer t
  :straight t)

(use-package atomic-chrome
  :defer t
  :straight t)

(use-package avy
  ;; Override minor mode binding for these.
  :bind* (
          ;; Binding xen-avy-goto-word-1 and xen-avy-goto-line on
          ;; use-package xen.
          ("M-u" . avy-goto-char-in-line)
          ("M-U" . avy-goto-char))
  :straight t)

(use-package avy-zap
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim))
  :straight t)

;; Used by magit, we'll delight it.
(use-package autorevert
  :commands auto-revert-mode
  :delight auto-revert-mode)

(use-package browse-kill-ring
  ;; Duplicate the effect of the advice that
  ;; browse-kill-ring-default-keybindings creates with a function.
  ;; This allows us to autoload on demand.
  :bind ("M-y" . (lambda (arg)
                   (interactive "p")
                   (if (not (eq last-command 'yank))
                       (browse-kill-ring)
                     (barf-if-buffer-read-only)
                     (yank-pop arg))))
  :straight t)

(use-package cask-mode
  :defer t
  :straight t)

(use-package css-mode
  :commands css-mode)

(use-package column-enforce-mode
  :commands column-enforce-mode
  :delight
  :hook drupal-mode
  :config
  (eval-after-load "doom-themes"
    (set-face-attribute 'column-enforce-face nil :inherit nil
                        :underline nil
                        :background (doom-darken 'warning .65)))
  :straight t)

(use-package company
  :commands global-company-mode
  :delight
  ;; Global mode, can't really be deferred, but delay it until
  ;; php-extras and xen-company has had a chance to define their
  ;; completers and functions.
  :after (php-extras xen-company)
  :defines company-semantic-modes
  :config
  (global-company-mode)
  ;; Use the TAB only frontend.
  (company-tng-configure-default)
  ;; Redefine tab to insert common prefix first.
  (define-key company-active-map [tab] 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)

  ;; Don't mess with up and down.
  (define-key company-active-map (kbd "<down>") nil)
  (define-key company-active-map (kbd "<up>") nil)

  ;; Use return to select in search mode (muscle memory is too used to
  ;; telling isearch that I'm done with return).
  (define-key company-search-map [return] 'company-complete-selection)
  (define-key company-search-map (kbd "RET") 'company-complete-selection)

  ;; Swap search and filter shortcuts.
  (define-key company-active-map "\C-s" 'company-filter-candidates)
  (define-key company-active-map "\C-\M-s" 'company-search-candidates)

  ;; TODO: this doesn't quite work, but it would be nice.
  ;; (defun company-preview-if-not-tng-frontend (command)
  ;;   "`company-preview-frontend', but not when tng is active."
  ;;   (unless (and (eq command 'post-command)
  ;;                company-selection-changed
  ;;                (memq 'company-tng-frontend company-frontends))
  ;;     (company-preview-frontend command)))
  ;; (setq company-frontends '(company-tng-frontend
  ;;                           company-preview-if-not-tng-frontend
  ;;                           company-pseudo-tooltip-frontend
  ;;                           company-echo-metadata-frontend))
  :straight t)

(use-package company-restclient
  :after (company restclient)
  :straight t)

(use-package counsel
  :defer t
  :straight t)

(use-package cov
  :hook ((emacs-lisp-mode . cov-mode)
         (php-mode . cov-mode))
  :straight t)

(use-package dap-mode
  :commands (dap-mode dap-ui-mode)
  :straight t)

(use-package dashboard
  :commands dashboard-setup-startup-hook
  :demand
  :defines (dashboard-startup-banner dashboard-item-generators dashboard-items)
  :config
  (setq dashboard-startup-banner 'logo)
  (add-to-list 'dashboard-item-generators '(xen-tip . xen-dashboard-tip))
  (add-to-list 'dashboard-item-generators '(xen-todo . xen-dashboard-todo))
  (setq dashboard-items '((projects . 20) (xen-tip) (xen-todo)))
  (dashboard-setup-startup-hook)
  :straight t)

(use-package diff-hl
  :commands global-diff-hl-mode
  :config (global-diff-hl-mode)
  :straight t)

(use-package dockerfile-mode
  :defer t
  :straight t)

(use-package drupal-mode
  :defer t
  :config
  :delight drupal-mode '(:eval (list " " (propertize (concat [#xF1A9])
                                                     'face '(:family "FontAwesome"))))
  :straight t)

;; Part of drupal-mode.
(use-package drush-make-mode
  :after drupal-mode)

(use-package ede-php-autoload
  :commands ede-php-autoload-mode
  :hook (php-mode . global-ede-mode)
  :straight t)

(use-package ede-php-autoload-composer-installers
  :after ede-php-autoload-mode
  :load-path "~/.emacs.d/ede-php-autoload-composer-installers/")

(use-package ede-php-autoload-drupal
  :after ede-php-autoload-mode
  :load-path "~/.emacs.d/ede-php-autoload-drupal/")

(use-package ecukes
  :commands ecukes
  :straight t)

;; For editing code blocks in Markdown mode.
(use-package edit-indirect
  :after markdown-mode
  :straight t)

(use-package eldoc
  :commands eldoc-mode
  :delight)

;; Core emacs stuff.
(use-package emacs
  :delight
  (auto-fill-function)
  (abbrev-mode)
  :hook (prog-mode . eldoc-mode)
  :config
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

(use-package expand-region
  :bind ("C-S-SPC" . er/expand-region)
  :hook (php-mode . xen-php-mode-expansions)
  ;; How it manages to load expand-region without requiring this is an
  ;; interesting question.
  :config (require 'er-basic-expansions)
  :straight t)

(use-package feature-mode
  :defer t
  :straight t)

(use-package fish-mode
  :defer t
  :straight t)

(use-package flycheck
  :bind (:map flycheck-mode-map
              ("M-<up>" . flycheck-previous-error)
              ("M-<down>" . flycheck-next-error))
  ;; Enable flycheck globally, doing it this way delays the setup to
  ;; after everything is loaded.
  :hook (after-init . global-flycheck-mode)
  :straight t)

(use-package flycheck-cask
  :commands flycheck-cask-setup
  :hook (flycheck-mode . flycheck-cask-setup)
  :straight t)

(use-package flycheck-color-mode-line
  :commands flycheck-color-mode-line-mode
  :hook (flycheck-mode . flycheck-color-mode-line-mode)
  :config
  (eval-after-load "doom-themes"
    '(progn
       (set-face-attribute 'flycheck-color-mode-line-error-face nil :inherit nil
                           :background (doom-darken 'error .60))
       (set-face-attribute 'flycheck-color-mode-line-warning-face nil :inherit nil
                           :background (doom-darken 'warning .70))
       (set-face-attribute 'flycheck-color-mode-line-info-face nil :inherit nil
                           :background (doom-darken 'blue .75))
       (set-face-attribute 'flycheck-color-mode-line-running-face nil :inherit nil
                           :background (doom-darken 'cyan .70))
       (set-face-attribute 'flycheck-color-mode-line-success-face nil
                           :background (doom-darken 'success .75))))
  :straight t)

(use-package flycheck-package
  :commands flycheck-package-setup
  :hook (flycheck-mode . flycheck-package-setup)
  :straight t)

(use-package flyspell
  :commands flyspell-mode
  :hook (((gfm-mode yaml-mode) . flyspell-mode)
         ((emacs-lisp-mode php-mode css-mode js-mode ruby-mode) . flyspell-prog-mode))
  :delight)

(use-package flyspell-correct-ivy
  ;; flyspell seems to overwrite its map when loaded, so defer to
  ;; after it's loaded.
  :after flyspell
  :bind (:map flyspell-mode-map
              ("C-$" . flyspell-correct-wrapper))
  :straight t)

(use-package forge
  :after magit
  :straight t)

;; http://www.emacswiki.org/emacs/FrameMove
(use-package framemove
  :config (setq framemove-hook-into-windmove t)
  :straight t)

(use-package git-attr
  :straight (:host github :repo "arnested/emacs-git-attr"))

(use-package google-this
  :commands google-this-mode
  :delight
  ;; Why does this use use-package-autoload-keymap?
  :bind-keymap ("C-c /" . google-this-mode-submap)
  :config (google-this-mode)
  :straight t)

(use-package ggtags
  ;; TODO: icon lighter
  :delight
  :commands ggtags-mode
  :straight t)

(use-package harvest
  :commands harvest-alist-get
  :bind ("C-c h" . harvest)
  ;; Override function to truncate project and client, so the comment
  ;; is visible.
  :config (defun harvest-format-entry (entry)
            "Format an ENTRY as a string.
Format is PROJECT (CLIENT) \n TASK - NOTES"
            (let ((formatted-string (concat
                                     (s-truncate 10 (harvest-alist-get '(project) entry))
                                     " ("
                                     (s-truncate 10 (harvest-alist-get '(client) entry))
                                     ")"
                                     ": "
                                     (harvest-alist-get '(task) entry)
                                     " - "
                                     (harvest-alist-get '(notes) entry)
                                     "\t["
                                     (number-to-string (harvest-alist-get '(hours) entry))
                                     "]"
                                     )))
              (if (harvest-alist-get '(timer_started_at) entry)
                  (propertize formatted-string 'face '(:background "green" :foreground "white"))
                (propertize formatted-string 'face 'nil))))
  :straight t)

(use-package highlight-symbol
  :commands highlight-symbol-mode
  :delight
  :hook ((emacs-lisp-mode php-mode css-mode js-mode ruby-mode) . highlight-symbol-mode)
  :bind (("M-<left>" . highlight-symbol-prev)
         ("M-<right>" . highlight-symbol-next))
  :straight t)

(use-package hl-line
  ;; Let xen-term-mode handle hl-line-mode toggling in term buffers.
  :hook (after-change-major-mode . (lambda ()
                                     (unless (eq major-mode 'term-mode)
                                       (hl-line-mode)))))

(use-package hungry-delete
  :delight
  :hook ((emacs-lisp-mode php-mode css-mode js-mode ruby-mode) . hungry-delete-mode)
  :straight t)

;; Checkout http://oremacs.com/2015/01/29/more-hydra-goodness/
;; Window commands: http://emacs.stackexchange.com/questions/3458/how-to-switch-between-windows-quickly
;; rotate package
(use-package hydra
  ;; defhydra expands to code using these.
  :commands (hydra-default-pre
             hydra-keyboard-quit
             hydra--call-interactively-remap-maybe
             hydra-show-hint
             hydra-set-transient-map)

  ;; Bind to prefix key, so the hint is shown immediately.
  :bind (("C-c w" . hydra-window/body)
         ("M-c" . hydra-case/body)
         ("C-z" . hydra-selection/body)
         ("C-)" . hydra-selection/body))
  ;; check out use-package-hydra
  :config
  (defhydra hydra-window (global-map "C-c w" :color pink)
    "window"
    ;; Dvorak.
    ("c" enlarge-window "enlarge")
    ("h" shrink-window-horizontally "widen")
    ("n" enlarge-window-horizontally "slim")
    ("t" shrink-window "shrink")
    ;; Qwerty.
    ("i" enlarge-window "enlarge")
    ("j" shrink-window-horizontally "widen")
    ("l" enlarge-window-horizontally "slim")
    ("k" shrink-window "shrink")
    ("q" nil "exit"))

  (defhydra hydra-case (global-map "M-c")
    "case"
    ("c" capitalize-word "Capitalize")
    ("u" upcase-word "UPPER")
    ("l" downcase-word "lower")
    ("s" string-inflection-underscore "lower_snake")
    ("n" string-inflection-upcase "UPPER_SNAKE")
    ("a" string-inflection-lower-camelcase "lowerCamel")
    ("m" string-inflection-camelcase "UpperCamel")
    ("k" string-inflection-kebab-case "kebab-case")
    )

  (defvar hydra-selection-cookie nil
    "Face remap cookie for hydra-selection/body")
  (defhydra hydra-selection
    (global-map "C-z" :color blue
                :pre (unless hydra-selection-cookie
                       (setq
                        hydra-selection-cookie (face-remap-add-relative
                                                'region :background "#48A")))
                :post (when hydra-selection-cookie
                        (face-remap-remove-relative hydra-selection-cookie)
                        (setq hydra-selection-cookie nil)))
    "selection"
    (";" comment-or-uncomment-region "Comment")
    ("<return>" mc/edit-lines "MC edit lines")
    ("?" count-words-region "Counts")
    ("A" ag "ag")
    ("a" xen-counsel-ag "counsel-ag")
    ("m" apply-macro-to-region-lines "Apply macro")
    ("q" nil "cancel")
    ("s" sort-lines "Sort")
    ("u" delete-duplicate-lines "De-dupe")
    )
  :straight t)

(use-package indentinator
  :hook ((emacs-lisp-mode cask-mode php-mode css-mode js-mode ruby-mode twig-mode) . indentinator-mode)
  :straight (:host github :repo "xendk/indentinator"))

;; Standard Emacs package. Dead keys work when this is loaded.
(use-package iso-transl)

;; Technically part of swiper, but we'll configure it here.
(use-package ivy
  :delight
  :commands (ivy-mode
             ivy-read
             ivy--switch-buffer-matcher
             ivy--switch-buffer-action
             ivy-call
             swiper)
  :init
  (ivy-mode 1)
  ;; Buffer switching with preview.
  :bind (:map ivy-mode-map
              ("M-x" . counsel-M-x)
              ("C-x C-f" . counsel-find-file)
              ("<f1> f" . counsel-describe-function)
              ("<f1> v" . counsel-describe-variable)
              ("<f1> l" . counsel-load-library)
              ("<f2> i" . counsel-info-lookup-symbol)
              ("<f2> u" . counsel-unicode-char)
              ("C-c a" . counsel-ag)
              :map ivy-minibuffer-map
              ("S-<return>" . ivy-immediate-done)
              ;; Like isearch.
              ("C-w" . ivy-yank-word)
              ;; Like isearch, repeating the key uses the last item
              ;; from the history and goes on from there.
              ("C-<tab>" . ivy-next-line-or-history)
              ;; For symmetry.
              ("C-<backtab>" . ivy-previous-line-or-history)))

(use-package ivy-hydra
  :straight t)

;; Properly handle annotations in java-mode.
(use-package java-mode-indent-annotations
  :load-path "~/.emacs.d/lib/"
  :commands java-mode-indent-annotations-setup
  :hook (java-mode . java-mode-indent-annotations-setup))

;; Built in.
(use-package js
  :commands js-mode)

(use-package keyfreq
  :if xen-primary
  :commands (keyfreq-mode keyfreq-autosave-mode)
  :init
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  :straight t)

(use-package lsp-mode
  :commands lsp
  ;; Can't add to company-backends before company has been loaded.
  :after company
  :init
  ;; Customize is somewhat broken for for me for lsp, so in the
  ;; meantime:
  (setq lsp-auto-configure nil)
  :hook ((lsp-mode . lsp-ui-mode)
         (lsp-mode . (lambda ()
                       (lsp-ui-flycheck-enable t)
                       ;; Not currently using imenu, but enable the
                       ;; support anyway.
                       (lsp-enable-imenu)
                       ;; Add lsp to backends rather than replacing
                       ;; it, so the existing completers will get a
                       ;; chance when lsp doesn't have any
                       ;; suggestions.
                       (add-to-list 'company-backends 'company-lsp)))
         ;; prog-mode?
         (php-mode . lsp))
  :config
  ;; This also depends on lsp-auto-configure, so we load them here.
  (when lsp-auto-require-clients
    (require 'lsp-clients))
  :straight t)

;; (flycheck-add-next-checker 'lsp-ui '(warning . php-phpcs))
;; lsp-ui-sideline-mode
(use-package lsp-ui
  :commands (lsp-ui-mode lsp-ui-sideline-mode)
  :config
  ;; Add phpcs as next-checker after lsp, so we get our PSR/Drupal
  ;; style checkers back.
  (flycheck-add-next-checker 'lsp-ui '(warning . php-phpcs))
  ;; Use sideline mode in all flycheck buffers. Better than displaying
  ;; in mini-buffer or flycheck-inline.
  (require 'lsp-ui-flycheck)
  :hook (flycheck-mode . lsp-ui-sideline-mode)

  :straight t)

(use-package company-lsp
  :commands company-lsp
  :straight t)

(use-package magit
  :defines magit-last-seen-setup-instructions
  :init (setq magit-last-seen-setup-instructions "1.4.0")
  :bind (;; Add shortcut to open magit status buffer.
         ("C-c g g" . magit-status)
         ;; This should really be in magit-file-mode-map, but as we've
         ;; turned C-g g into a prefix map above, that wont work. So
         ;; just make them global.
         ("C-c g d" . magit-dispatch)
         ("C-c g f" . magit-file-dispatch))
  :hook (git-commit-setup . xen-git-commit-setup)
  :config
  ;; Let's remove these extremely difficult key combinations. We've
  ;; bound the functions above.
  (unbind-key "\C-x\M-g" magit-file-mode-map)
  (unbind-key "\C-c\M-g" magit-file-mode-map)
  ;; Add --follow-tags options to the push popup.
  ;; Delight has better handling for major-modes.
  (delight 'magit-status-mode
           (propertize (concat " " [#xF1D3])
                       'face '(:family "FontAwesome")) :major)
  :straight t)

;; Add git flow extension.
(use-package magit-gitflow
  :after magit
  :delight
  :hook (magit-mode . turn-on-magit-gitflow)
  :straight t)

(use-package magit-filenotify
  :defer t
  :straight t)

(use-package magithub
  :commands magithub-feature-autoinject
  :after magit
  :config (magithub-feature-autoinject t)
  :straight t)

(use-package markdown-mode
  :mode (("\\.\\(m\\(ark\\)?down\\)$" . markdown-mode)
         ("\\.md$" . gfm-mode))
  :hook ((gfm-mode . auto-fill-mode))
  :straight t)

(use-package mwim
  :bind (("C-a" . mwim-beginning)
         ("C-e" . mwim-end))
  :straight t)

;; From http://www.gerd-neugebauer.de/software/emacs/multi-mode/multi-mode.el
(use-package multi-mode
  :load-path "~/.emacs.d/lib/"
  :commands multi-mode)
;; Example:
;; (multi-mode 1 'html-mode '("<?php" php-mode) '("?>" html-mode))

(use-package multi-term
  :commands (multi-term multi-term-dedicated-exist-p term-send-raw-string term-line-mode fish)
  :bind (:map term-mode-map
              ("RET" . term-char-mode))
  :config
  (defun term-send-alt-up    () (interactive) (term-send-raw-string "\e[1;3A"))
  (defun term-send-alt-down  () (interactive) (term-send-raw-string "\e[1;3B"))
  (defun term-send-alt-right () (interactive) (term-send-raw-string "\e[1;3C"))
  (defun term-send-alt-left  () (interactive) (term-send-raw-string "\e[1;3D"))
  (defun term-send-ctrl-right () (interactive) (term-send-raw-string "\e[1;5C"))
  (defun term-send-ctrl-left  () (interactive) (term-send-raw-string "\e[1;5D"))
  (defun term-send-alt-backspace  () (interactive) (term-send-raw-string "\e\C-?"))
  (defun term-send-yank  () (interactive) (term-send-raw-string "\C-y"))
  (defun term-swiper  () (interactive) (term-line-mode) (swiper))
  (defun term-isearch-backward  () (interactive) (term-line-mode) (isearch-backward))
  (defun term-avy-goto-word-1  () (interactive) (term-line-mode) (call-interactively 'avy-goto-word-1))
  (defun term-xen-avy-goto-line  () (interactive) (term-line-mode) (xen-avy-goto-line))
  (defun fish ()
    "Open a fish shell buffer."
    (interactive)
    (let ((multi-term-program "fish")
          (multi-term-buffer-name "fish "))
      (multi-term)))
  :straight t)

(use-package multiple-cursors
  :bind (
         ("C-<" . mc/mark-previous-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-M-m" . mc/mark-more-like-this-extended)
         ("C-*" . mc/mark-all-like-this)
         ("C-%" . mc/mark-all-in-region)
         ("C-=" . mc/mark-all-like-this-dwim))
  :straight t)

;; We're using the built in version of org. Upgrading it requires some hackery:
;; https://github.com/raxod502/radian/blob/ee92ea6cb0473bf7d20c6d381753011312ef4a52/radian-emacs/radian-org.el#L46-L112
;; And as we're quite content with it, we're sticking with the built in version.
(use-package org-mode
  :commands org-mode
  :mode "\\.org\\'")

;; package-lint requires package for its package database. So we defer
;; it and use :config to initialize it when someone requires it.
(use-package package
  :commands package-initialize
  :defer t
  :config
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize))

(use-package page-break-lines
  :delight
  :hook (emacs-lisp-mode . page-break-lines-mode)
  :straight t)

(use-package php-boris
  :commands php-boris
  :straight t)

(use-package php-mode
  :commands php-mode
  :config
  (require 'dap-php)
  :straight t)

(use-package php-extras
  :straight (:host github :repo "arnested/php-extras"))

(use-package projectile
  :commands (projectile-mode projectile-project-p)
  :delight ""
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (projectile-mode)
  :straight t)

(use-package rainbow-mode
  :straight t)

(use-package rjsx-mode
  :commands rjsx-mode
  :mode "components\\/.*\\.js\\'"
  :straight t)

(use-package ruby-mode
  :commands ruby-mode)

(use-package s
  :commands s-truncate
  :straight t)

;; Built in, but we need to activate it.
(use-package saveplace
  :init (save-place-mode))

;; Figure this one out.
;; (use-package semantic-php
;;   :commands semantic-mode
;;   :load-path "~/.emacs.d/semantic-php/"
;;   :init
;;   (add-hook 'php-mode-hook #'semantic-mode)
;;   :config
;;   (load "~/.emacs.d/semantic-php/loaddefs.el")
;;   (add-to-list 'company-semantic-modes 'php-mode)
;;   )

(use-package smartparens
  :commands (smartparens-mode smartparens-global-mode show-smartparens-global-mode sp-pair sp-local-pair)
  :demand
  :delight
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1)
  ;; Don't autopair ' when after a word (makes the first word of this
  ;; sentence difficult to type).
  (sp-pair "'" nil :unless '(sp-point-after-word-p))
  ;; When pressing return as the first thing after inserting
  ;; a { or (, add another and indent.
  (sp-local-pair 'php-mode "{" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair 'php-mode "(" nil :post-handlers '(("||\n[i]" "RET")))

  (sp-local-pair 'php-mode "/*" "*/" :actions '(wrap insert)
                 :post-handlers '(("* ||\n[i]" "RET") ("\n[i]* ||\n[i]" "*")))

  (sp-local-pair 'js-mode "/*" "*/" :actions '(wrap insert)
                 :post-handlers '(("* ||\n[i]" "RET") ("\n[i]* ||\n[i]" "*")))

  (sp-local-pair 'css-mode "/*" "*/" :actions '(wrap insert)
                 :post-handlers '(("* ||\n[i]" "RET")))

  (sp-local-pair 'twig-mode "{" nil :actions nil)
  (sp-local-pair 'twig-mode "{{" "}}" :actions '(wrap insert))
  (sp-local-pair 'twig-mode "{%" "%}" :actions '(wrap insert))
  (sp-local-pair 'twig-mode "{#" "#}" :actions '(wrap insert))
  ;; Hmm, no workie.
  ;; (eval-after-load "twig-mode"      '(require 'smartparens-html))
  ;; (eval-after-load "smartparens" '(sp-local-tag  'twig-mode "<" "<_>" "</_>" :transform 'sp-match-sgml-tags :post-handlers '(sp-html-post-handler)))
  ;; (require 'smartparens-html)
  :straight t)

(use-package smex
  ;; autoload when needed.
  :defer
  :straight t)

(use-package solaire-mode
  :hook (((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
         (minibuffer-setup . solaire-mode-in-minibuffer))
  :config
  (solaire-mode-swap-bg)
  :straight t)

(use-package string-inflection
  ;; autoload when needed.
  :commands (string-inflection-underscore
             string-inflection-upcase
             string-inflection-lower-camelcase
             string-inflection-camelcase
             string-inflection-kebab-case)
  :straight t)

(use-package systemd
  :defer t
  :straight t)

;; Built in.
(use-package term
  :defer
  :bind (:map term-raw-map
              ;; Get paste working in (multi-)term-mode.
              ("C-y" . term-paste)))

;; Use terraform-mode for Github Actions workflow files.
(use-package terraform-mode
  :mode "\\.workflow\\'"
  :straight t)

(use-package twig-mode
  :defer t
  :straight t)

(use-package undo-tree
  :commands global-undo-tree-mode
  :demand
  :delight
  :init (global-undo-tree-mode)
  :bind (:map undo-tree-visualizer-mode-map
              ;; Make return accept currently selected revision and q
              ;; (and C-g) abort. The defaults are weird.
              ("<return>" . undo-tree-visualizer-quit)
              ("C-g" . undo-tree-visualizer-abort)
              ("q" . undo-tree-visualizer-abort))
  ;; Pull package directly from maintainer, the elpa package is behind.
  :straight (undo-tree :type git :host nil :repo "http://www.dr-qubit.org/git/undo-tree.git"))

(use-package vcl-mode
  :commands vcl-mode
  :mode "\\.vcl\\'"
  :straight t)

(use-package watch-buffer
  :commands watch-buffer
  :straight t)

(use-package visual-fill-column
  :commands visual-fill-column-mode
  :straight t)

(use-package visual-regexp
  :bind (("C-c r" . vr/replace)
         ("C-c q" . vr/query-replace)
         ("C-c m" . vr/mc-mark))
  :straight t)

;; Writable grep buffer.
;; (use-package wgrep
;;   :straight t)

;; http://www.emacswiki.org/emacs/WindMove
;; Built in.
(use-package windmove
  :bind* (("<S-up>" . windmove-up)
          ("<S-down>" . windmove-down)
          ("<S-left>" . windmove-left)
          ("<S-right>" . windmove-right))
  ;; Make windmove work in org-mode:
  :hook ((org-shiftup-final  . windmove-up)
         (org-shiftleft-final  . windmove-left)
         (org-shiftdown-final  . windmove-down)
         (org-shiftright-final  . windmove-right)))

;; http://www.emacswiki.org/emacs/WinnerMode
;; Built in.
(use-package winner
  :config (winner-mode))

(use-package ws-butler
  :commands ws-butler-mode
  :delight
  :hook ((emacs-lisp-mode php-mode ruby-mode css-mode js-mode feature-mode) . ws-butler-mode)
  :straight t)

(use-package xen
  :load-path "~/.emacs.d/xen/"
  :demand
  ;;:functions xen-coding-common-bindings
  :hook ((emacs-lisp-mode php-mode css-mode js-mode ruby-mode) . xen-coding-common-bindings)
  :bind* ("S-SPC" . xen-avy-goto-word-1)
  :bind (("M-SPC" . xen-cycle-spacing)
         ("M-l" . xen-avy-goto-line)
         ("<f12>" . xen-big-fringe-mode)
         ("C-S-d" . xen-duplicate-current-line)
         ;; ("C-!" . xen-multi-term-dedicated-toggle-and-select)
         ("C-S-l" . xen-mark-lines)
         ("C-c x" . xen-map)))

(use-package xen-company
  :load-path "~/.emacs.d/xen/")

(use-package xen-flycheck
  :load-path "~/.emacs.d/xen/"
  :functions xen-flycheck-mode-line-status-text)

(use-package xen-projectile
  :load-path "~/.emacs.d/xen/"
  :after (projectile ivy)
  :bind (:map projectile-command-map
              ("s" . xen-projectile-switch-to-shell)
              ("S" . fish)))

(use-package xen-smartparens
  :load-path "~/.emacs.d/xen/"
  :after (smartparens))

(use-package xen-swiper
  :load-path "~/.emacs.d/xen/"
  :commands xen-counsel-ag
  :after (ivy)
  :bind (("C-<tab>" . xen-swiper)
         :map isearch-mode-map
         ("C-<tab>" . xen-swiper-from-isearch)
         :map ivy-mode-map
         ("C-x b" . xen-switch-buffer)))

(use-package xen-term-mode
  :load-path "~/.emacs.d/xen/"
  ;; Enforce loading so advices get applied when term-mode loads.
  :demand t
  :bind (:map term-mode-map
              ("S-<return>" . xen-term-mode-yank-into-char-mode))
  :after (multi-term))

(use-package yaml-mode
  :mode "\\.e?ya?ml$"
  :straight t)

(use-package yasnippet
  :commands yas-reload-all
  :delight yas-minor-mode
  :hook ((emacs-lisp-mode php-mode css-mode js-mode ruby-mode) . yas-minor-mode)
  :config (yas-reload-all)
  :straight t)



;;; Some places for inspiration

;; https://github.com/tomjakubowski/.emacs.d/blob/master/init.el
;; http://www.aaronbedra.com/emacs.d/
;; http://batsov.com/articles/2011/11/30/the-ultimate-collection-of-emacs-resources/
;; https://www.youtube.com/watch?v=p3Te_a-AGqM&feature=player_embedded#!
;; http://emacsrocks.com/

(provide 'init)
;;; init.el ends here
