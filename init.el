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

;;; Code:

;; Quick debugging:
;; (toggle-debug-on-error)

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

;;; Aliases and advices
;; I'm grown up, I can manage using y/n for even destructive commands.
(defalias 'yes-or-no-p 'y-or-n-p)

(defadvice show-paren-function (after my-echo-paren-matching-line activate)
  "If a matching paren is off-screen, echo the matching line."
  (when (char-equal (char-syntax (char-before (point))) ?\))
    (let ((matching-text (blink-matching-open)))
      (when matching-text
        (message matching-text)))))



;; Initialize package system.
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("marmalade" .
               "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; Bindings.
;; Used to M-DEL deleting a word.
(bind-key "M-<delete>" 'kill-word)

;; Don't iconify on C-z.
(unbind-key "C-z")

;; Maybe reintroduce these now that I'm using the same keys in Terminator?
;; http://www.emacswiki.org/emacs/WindowResize
;; (global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
;; (global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
;; (global-set-key (kbd "S-C-<down>") 'shrink-window)
;; (global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; Quickly open URLs.
(bind-key "C-c b" 'browse-url-at-point)

;;; Packages.

(use-package avy
  :commands avy-goto-word-1
  :bind (("S-SPC" . avy-goto-word-1)
         ("M-u" . avy-goto-char-in-line)
         ("M-U" . avy-goto-char)
         ;; Binding xen-avy-goto-line on use-package xen.
         )
  :init (progn
          (eval-after-load 'cus-edit
          '(bind-key "S-SPC" 'avy-goto-word-1 custom-mode-map))))

(use-package avy-zap
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim)))

(use-package auto-indent-mode
  :commands auto-indent-mode auto-indent-global-mode
  :diminish auto-indent-mode
  :init (auto-indent-global-mode))

(use-package browse-kill-ring
  :config (browse-kill-ring-default-keybindings))

(use-package css-mode
  :commands css-mode
  :config (add-hook
           'css-mode-hook
           (lambda () (xen-coding-common-bindings)
             (yas-minor-mode 1))))

(use-package column-enforce-mode
  :commands column-enforce-mode
  :diminish column-enforce-mode)

(use-package company
  :diminish ""
  ;; Global mode, can't really be deferred.
  :demand
  :defines company-semantic-modes
  :bind (:map company-active-map
              ;; company-complete-common is annying as it only completes
              ;; the common part, company-complete-selection always
              ;; selects the first option and company-complete requires
              ;; double tabs all the times.
              ;; This completes the common part or selects the first (or selected) option.
              ("TAB" . xen-company-complete-common-or-selection)
              ("<tab>" . xen-company-complete-common-or-selection)
              )
  :config (progn
            (global-company-mode)
            ;; Remove enter key-binding, it messes with normal typing.
            (unbind-key "RET" company-active-map)
            (unbind-key "<return>" company-active-map)
            ;; Define a local map that's only active when selection
            ;; has changed, and bind return to the action we unbound
            ;; it from in the normal keymap. This means we can use
            ;; return to select the chosen item, but it wont mess with
            ;; normal typing.
            (defvar xen-company-explicit-map (make-sparse-keymap))
            (bind-key "RET" 'company-complete-selection xen-company-explicit-map)
            (add-to-list 'minor-mode-map-alist (cons 'company-selection-changed
                                                     xen-company-explicit-map))
            ))

(use-package diff-hl
  :config (global-diff-hl-mode))

(use-package drupal-mode
  :load-path "~/.emacs.d/drupal-mode/"
  :config (progn
            ;; drupal-mode-beginning-of-line doesn't play ball with
            ;; auto-indent-mode, and the only thing it adds is the 'go
            ;; to the start of value' thing for conf-mode. As Drupal 8
            ;; uses YAML, we'll live without.
            (unbind-key "C-a" drupal-mode-map)
            (add-hook 'drupal-mode-hook (lambda () (column-enforce-mode)))
            (delight 'drupal-mode
                     (propertize (concat " " [#xF1A9])
                                 'face '(:family "FontAwesome")))))

(use-package drush-make-mode)

(use-package ede-php-autoload-mode
  :commands ede-php-autoload-mode
  :load-path "~/.emacs.d/ede-php-autoload/"
  :init (add-hook 'php-mode-hook 'ede-php-autoload-mode))

(use-package ecukes
  :commands ecukes)

(use-package eldoc
  :commands eldoc-mode
  :diminish "")

(use-package expand-region
  :bind ("C-S-SPC" . er/expand-region)
  :config (add-hook 'php-mode-hook 'xen-php-mode-expansions))

(use-package flycheck
  :bind (:map flycheck-mode-map
              ("M-<up>" . flycheck-previous-error)
              ("M-<down>" . flycheck-next-error))
  ;; Enable flycheck globally, doing it this way delays the setup to
  ;; after everything is loaded.
  :init (add-hook 'after-init-hook #'global-flycheck-mode)
  :config (progn (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))

(use-package flyspell
  :commands flyspell-mode
  :diminish "")

(use-package flyspell-correct-ivy
  :bind (:map flyspell-mode-map
              ("C-;" . flyspell-correct-previous-word-generic)))

;; http://www.emacswiki.org/emacs/FrameMove
(use-package framemove
  :config (setq framemove-hook-into-windmove t))

(use-package google-this
  :diminish google-this-mode
  :bind-keymap ("C-c /" . google-this-mode-submap)
  :config (google-this-mode))

(use-package ggtags
  ;; @todo icon lighter
  :diminish ggtags-mode
  :commands ggtags-mode
  )

(use-package harvest
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
                (propertize formatted-string 'face 'nil)))))

(use-package highlight-symbol
  :commands highlight-symbol-mode
  :diminish "")

(use-package hl-line
  :config (global-hl-line-mode))

;; Checkout http://oremacs.com/2015/01/29/more-hydra-goodness/
;; Window commands: http://emacs.stackexchange.com/questions/3458/how-to-switch-between-windows-quickly
;; rotate package
(use-package hydra
  ;; TODO: use :config and :bind.
  :init (progn
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
          (bind-key "C-c w" 'hydra-window/body)

          (defhydra hydra-case (global-map "M-c")
            "case"
            ("c" capitalize-word "Capitalize")
            ("u" upcase-word "UPPER")
            ("l" downcase-word "lower")
            ("s" string-inflection-underscore "lower_snake")
            ("n" string-inflection-upcase "UPPER_SNAKE")
            ("a" string-inflection-lower-camelcase "lowerCamel")
            ("m" string-inflection-camelcase "UpperCamel")
            ("d" string-inflection-lisp "dash-case")
            )
          (bind-key "M-c" 'hydra-case/body)))

;; Standard Emacs package. Dead keys work when this is loaded.
(use-package iso-transl)

;; Technically part of swiper, but we'll configure it here.
(use-package ivy
  :diminish ""
  :init (ivy-mode 1)
  :bind (:map ivy-mode-map
         ("C-s" . swiper)
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
         ("C-S-s" . ivy-yank-word)))

;; Properly handle annotations in java-mode.
(use-package java-mode-indent-annotations
  :load-path "~/.emacs.d/lib/"
  :commands java-mode-indent-annotations-setup
  :init (add-hook 'java-mode-hook 'java-mode-indent-annotations-setup))

(use-package js
  :commands js-mode
  :config (add-hook
           'js-mode-hook
           (lambda () (xen-coding-common-bindings)
             (yas-minor-mode 1))))

(use-package keyfreq
  :init (progn
          (keyfreq-mode 1)
          (keyfreq-autosave-mode 1)))

(use-package lisp-mode
  :commands emacs-lisp-mode
  :config (add-hook
           'emacs-lisp-mode-hook
           (lambda () (xen-coding-common-bindings)
             (yas-minor-mode 1))))

(use-package magit
  :defines magit-last-seen-setup-instructions
  :init (setq magit-last-seen-setup-instructions "1.4.0")
  :bind (;; Add shortcut to open magit status buffer.
         ("C-c C-g" . magit-status))
  :config (progn
             (use-package magithub)
             ;; Add git flow extension.
             (use-package magit-gitflow
               :diminish magit-gitflow-mode
               :init (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))
             (delight 'magit-status-mode
                      (propertize (concat " " [#xF1D3])
                                  'face '(:family "FontAwesome")) :major)))

(use-package markdown-mode
  :mode (("\\.\\(m\\(ark\\)?down\\)$" . markdown-mode)
         ("\\.md$" . gfm-mode))
  :config
  (progn
    (add-hook 'gfm-mode-hook
              '(lambda ()
                 (auto-fill-mode)
                 (flyspell-mode)))))

(use-package multi-term
  :commands multi-term-dedicated-exist-p)

(use-package multiple-cursors
  :bind (
         ("C-<" . mc/mark-previous-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-M-m" . mc/mark-more-like-this-extended)
         ("C-*" . mc/mark-all-like-this)
         ("C-%" . mc/mark-all-in-region)
         ("C-=" . mc/mark-all-like-this-dwim)))

(use-package org-mode
  :commands org-mode
  :mode "\\.org\\'")

(use-package paradox
  :commands (package-list-packages
             paradox-list-packages
             hydra-launch/paradox-list-packages-and-exit)
  :config
  (progn
    ;; The "paradox-token.el" file is supposed to contain this line:
    ;;     (setq paradox-github-token "<YOUR_TOKEN>")
    (load (locate-user-emacs-file "paradox-token.el") :noerror :nomessage)
    (paradox-enable)))

(use-package php-boris
  :commands php-boris)

(use-package php-mode
  :commands php-mode
  :config (add-hook
           'php-mode-hook
           (lambda () (xen-coding-common-bindings)
             (yas-minor-mode 1)
             (ggtags-mode)
             )))

(use-package projectile
  :commands projectile-project-p
  :diminish projectile-mode
  :init (progn
    (projectile-mode)))

(use-package ruby-mode
  :commands ruby-mode
  :config (add-hook
           'ruby-mode-hook
           (lambda () (xen-coding-common-bindings)
             (yas-minor-mode 1))))

(use-package semantic-php
  :load-path "~/.emacs.d/semantic-php/"
  :init (progn (load "~/.emacs.d/semantic-php/loaddefs.el")
               (add-hook 'php-mode-hook #'semantic-mode))
  :config (add-to-list 'company-semantic-modes 'php-mode)
  )

;; prog-mode is defined in simple.el.
(use-package simple
  :config (progn
            (add-hook 'prog-mode-hook #'eldoc-mode)
            ;; Emacs 24 changed the region highlight from a hackery
            ;; face thingy to a proper overlay. Which is fine apart
            ;; from giving it a nil priority which puts it below
            ;; pretty much everything else. So we redefine the
            ;; redisplay-highlight-region-function to give the overlay
            ;; a higher priority.
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
                      rol)))))

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

(use-package string-inflection
  ;; autoload when needed.
  :defer)

(use-package term
  :defer
  :bind (:map term-raw-map
              ;; Get paste working in (multi-)term-mode.
              ("C-y" . term-paste)))

(use-package undo-tree
  :diminish undo-tree-mode
  :init (progn
          (global-undo-tree-mode)))

(use-package vcl-mode
  :commands vcl-mode
  :mode "\\.vcl\\'")

; Writable grep buffer.
(use-package wgrep)

;; http://www.emacswiki.org/emacs/WindMove
(use-package windmove
  :config (progn
            (windmove-default-keybindings)
             ;; Make windmove work in org-mode:
            (add-hook 'org-shiftup-final-hook 'windmove-up)
            (add-hook 'org-shiftleft-final-hook 'windmove-left)
            (add-hook 'org-shiftdown-final-hook 'windmove-down)
            (add-hook 'org-shiftright-final-hook 'windmove-right)))

;; http://www.emacswiki.org/emacs/WinnerMode
(use-package winner
  :config (progn (winner-mode)))

(use-package ws-butler
  :commands ws-butler-mode
  :delight ws-butler-mode
  :init (add-hook 'php-mode-hook 'ws-butler-mode))

(use-package xen
  :load-path "~/.emacs.d/xen/"
  :demand
  :functions xen-coding-common-bindings
  :bind (("M-l" . xen-avy-goto-line)
         ;; Can we get 'n' to jump to $nid in php-mode? $ counts as
         ;; part of words in php-mode, that's the issue.
         ("C-<tab>" . xen-swiper)
         ("<f11>" . xen-toggle-fullscreen)
         ("S-<f11>" . xen-toggle-font-size)
         ("<f12>" . xen-big-fringe-mode)
         ("C-S-d" . xen-duplicate-current-line)
         ("C-!" . xen-multi-term-dedicated-toggle-and-select)
         ("C-S-l" . xen-mark-lines)))

(use-package yaml-mode
  :mode "\\.e?ya?ml$"
  :config (flyspell-mode))

(use-package yasnippet
  :diminish yas-minor-mode
  :config (yas-reload-all))

(use-package zeal-at-point
  :bind (("C-c d" . zeal-at-point)))



;;; Some places for inspiration

;; Mode line setup: http://www.lunaryorn.com/2014/07/26/make-your-emacs-mode-line-more-useful.html

;; https://github.com/geerds/emacs.d/blob/master/init.el
;; https://github.com/tomjakubowski/.emacs.d/blob/master/init.el
;; http://www.aaronbedra.com/emacs.d/

;; http://batsov.com/articles/2011/11/30/the-ultimate-collection-of-emacs-resources/

;;; Old comments left around...

; Try out http://www.emacswiki.org/emacs/MiniMap ?

;; TODO: https://github.com/rolandwalker/fixmee

;; TODO: CamelCase <-> snake_case conversion:
;; https://gist.github.com/846766
;; https://github.com/emacsmirror/s

; Really cool stuff: https://github.com/magnars
; From these:
; https://www.youtube.com/watch?v=p3Te_a-AGqM&feature=player_embedded#!
; http://emacsrocks.com/


; Todo: http://www.emacswiki.org/cgi-bin/wiki.pl?ColorTheme

; todo: http://stackoverflow.com/questions/892378/how-do-i-fix-the-cursor-to-the-middle-of-the-screen-in-emacs-so-that-the-page-m
; http://www.emacswiki.org/emacs/centered-cursor-mode.el

; Figure out how to ensure 80 cols with this.
; (global-linum-mode 1)

;; In every buffer, the line which contains the cursor will be fully
;; highlighted

(provide 'init)
;;; init.el ends here
