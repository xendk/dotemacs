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
;; rainbow-mode


;; Make sure that delight is available as soon as any package triggers it.
(use-package delight
  :commands delight
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
  :commands browse-kill-ring-default-keybindings
  ;; browse-kill-ring-default-keybindings doesn't really set up a
  ;; keybinding, but advices yank-pop, so we can't use :bind to
  ;; lazy-load it.
  :demand t
  :config (browse-kill-ring-default-keybindings)
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
  :straight t)

(use-package company
  :commands global-company-mode
  :delight
  ;; Global mode, can't really be deferred, but delay it until
  ;; php-extras has had a chance to define it's completer.
  :after php-extras
  :defines company-semantic-modes
  :init
  (global-company-mode)
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
  :straight (:host github :repo "xendk/cov" :branch "undercover-support"))

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
  :bind ("M-SPC" . xen-cycle-spacing)
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
            rol)))
  (defun xen-cycle-spacing (&optional n)
    "Delete all spaces and tabs around point, leaving one space (or N spaces).
If N is negative, delete newlines as well, leaving -N spaces.

Subsequent calls will delete all spaces, or revert to the original spacing.

See also `cycle-spacing'."
    (interactive "*p")
    (cycle-spacing n nil 'fast)))

(use-package expand-region
  :bind ("C-S-SPC" . er/expand-region)
  :hook (php-mode . xen-php-mode-expansions)
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
  :bind (:map flyspell-mode-map
              ("C-;" . flyspell-correct-previous-word-generic))
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
  :config (global-hl-line-mode))

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
  ;; TODO: use :config and :bind.
  :bind (("C-c w" . hydra-window/body)
         ("M-c" . hydra-case/body))
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
    ("q" nil "cancel"))
  ;; Bind to prefix key, so the hint is shown immediately.

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
  :straight t)

(use-package indentinator
  :hook ((emacs-lisp-mode cask-mode php-mode css-mode js-mode ruby-mode twig-mode) . indentinator-mode)
  :straight (:host github :repo "xendk/indentinator"))

;; Build in, but add some bindings.
(use-package isearch
  :commands isearch-backward)

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
              ("C-c g" . counsel-git)
              ("C-c a" . counsel-ag)
              :map ivy-minibuffer-map
              ("S-<return>" . ivy-immediate-done)
              ;; Like isearch.
              ("C-w" . ivy-yank-word)
              ;; Like isearch, repeating the key uses the last item
              ;; from the history and goes on from there.
              ("C-<tab>" . ivy-next-line-or-history)
              ;; For symmetry.
              ("C-<iso-lefttab>" . ivy-previous-line-or-history)))

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
  :disabled
  :config
  (lsp-define-stdio-client
   ;; This can be a symbol of your choosing. It will be used as a the
   ;; prefix for a dynamically generated function "-enable"; in this
   ;; case: lsp-prog-major-mode-enable
   lsp-php-mode
   "php"
   ;; This will be used to report a project's root directory to the LSP
   ;; server.
   (lambda () "/home/xen/sites/samvirke")
   ;; This is the command to start the LSP server. It may either be a
   ;; string containing the path of the command, or a list wherein the
   ;; car is a string containing the path of the command, and the cdr
   ;; are arguments to that command.
   '("/usr/bin/php" "/home/xen/dev/php-language-server/vendor/felixfbecker/language-server/bin/php-language-server.php"))

  ;; Here we'll add the function that was dynamically generated by the
  ;; call to lsp-define-stdio-client to the major-mode hook of the
  ;; language we want to run it under.
  ;;
  ;; This function will turn lsp-mode on and call the command given to
  ;; start the LSP server.
  (add-hook 'php-mode #'lsp-php-mode-enable)
  :straight t)

(use-package company-lsp
  :disabled
  :straight t)

(use-package magit
  :commands magit-define-popup-switch
  :defines magit-last-seen-setup-instructions
  :init (setq magit-last-seen-setup-instructions "1.4.0")
  :bind (;; Add shortcut to open magit status buffer.
         ("C-c C-g" . magit-status))
  :config
  ;; Add --follow-tags options to the push popup.
  (magit-define-popup-switch 'magit-push-popup
                             ?t "Follow tags" "--follow-tags")
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
  :straight t)

(use-package php-extras
  :straight (:host github :repo "arnested/php-extras"))

(use-package projectile
  :commands (projectile-mode projectile-project-p)
  :delight ""
  :init
  (projectile-mode)
  ;; Show current project next to the buffer name in the mode-line.
  (add-hook 'find-file-hook
            (lambda ()
              (setq mode-line-buffer-identification
                    (append mode-line-buffer-identification
                            '((:eval (if (projectile-project-p)
                                         (concat " [" (projectile-project-name) "]")
                                       "")))))))
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
  :straight t)

(use-package vcl-mode
  :commands vcl-mode
  :mode "\\.vcl\\'"
  :straight t)

(use-package watch-buffer
  :commands watch-buffer
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
  :bind (("M-l" . xen-avy-goto-line)
         ("<f12>" . xen-big-fringe-mode)
         ("C-S-d" . xen-duplicate-current-line)
         ;; ("C-!" . xen-multi-term-dedicated-toggle-and-select)
         ("C-S-l" . xen-mark-lines)
         ("C-c x" . xen-map)))

(use-package xen-company
  :load-path "~/.emacs.d/xen/"
  :after (company php-mode)
  ;; Enforce loading when company and php-mode has loaded, else the
  ;; :bind will defer loading until
  ;; xen-company-complete-common-or-selection is needed which means
  ;; that the unbinds in :config wont be run until then.
  :demand t
  :bind (:map company-active-map
              ;; company-complete-common is annying as it only completes
              ;; the common part, company-complete-selection always
              ;; selects the first option and company-complete requires
              ;; double tabs all the times.
              ;; This completes the common part or selects the first (or selected) option.
              ("TAB" . xen-company-complete-common-or-selection)
              ("<tab>" . xen-company-complete-common-or-selection))
  :config
  ;; Remove enter key-binding, it messes with normal typing.
  (unbind-key "RET" company-active-map)
  (unbind-key "<return>" company-active-map)
  ;; Define a local map that's only active when selection has changed,
  ;; and bind return to the action we unbound it from in the normal
  ;; keymap. This means we can use return to select the chosen item,
  ;; but it wont mess with normal typing.
  (defvar xen-company-explicit-map (make-sparse-keymap))
  (bind-key "RET" 'company-complete-selection xen-company-explicit-map)
  (add-to-list 'minor-mode-map-alist (cons 'company-selection-changed
                                           xen-company-explicit-map)))

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
  :after (ivy)
  :bind (("C-<tab>" . xen-swiper)
         :map isearch-mode-map
         ("C-<tab>" . xen-swiper-from-isearch)
         :map ivy-mode-map
         ("C-x b" . xen-switch-buffer)))

(use-package xen-term-mode
  :load-path "~/.emacs.d/xen/"
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
