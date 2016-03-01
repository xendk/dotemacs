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

;; Add ~/.emacs.d/lib to load-path.
(add-to-list 'load-path "~/.emacs.d/lib/")

;;; Bindings.
;(require 'bind-key)
;; TODO: use bind-key.
(define-key global-map [delete] 'delete-forward-char)
(define-key global-map [M-delete] 'kill-word)
(define-key global-map (kbd "C-S-Z") 'repeat)

;; Don't iconify on C-z.
(global-unset-key (kbd "C-z"))
;; (when (display-graphic-p)
;;   (unbind-key "C-z"))

;; todo: move xen-bindings to use-package...
(global-set-key [f11] 'xen-toggle-fullscreen)
(global-set-key [S-f11] 'xen-toggle-font-size)
(global-set-key [f12] 'xen-big-fringe-mode)
(global-set-key (kbd "C-a") 'xen-back-to-indentation-or-beginning)

;; Add shortcut to open magit status buffer.
(global-set-key (kbd "C-c C-g") 'magit-status)
;; (define-key global-map (kbd "C-x C-f") 'xen-find-file-dwim)

;; http://www.emacswiki.org/emacs/WindowResize
;; (global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
;; (global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
;; (global-set-key (kbd "S-C-<down>") 'shrink-window)
;; (global-set-key (kbd "S-C-<up>") 'enlarge-window)

(global-set-key (kbd "C-S-d") 'xen-duplicate-current-line)

;; As I never use C-v anyway, and its effect when i hit it confuses
;; me, why not bind it to pasting from outside (like the middle button
;; does)?
(global-set-key (kbd "C-v") 'xen-paste)
;; Also CTRL Shift v (to mirror xen-copy), which is implicit in the
;; above if not specifically bound, but let's make it explicit.
(global-set-key (kbd "S-C-v") 'xen-paste)

;; CRTL C is taken, so use CTRL Shift c like Gnome Terminal does, in
;; order to limit the amount of different key combinations I should
;; remember for the same thing.
(global-set-key (kbd "S-C-c") 'xen-copy)

(global-set-key [?\C-x ?\C-b] 'buffer-menu)

(global-set-key (kbd "C-!") 'xen-multi-term-dedicated-toggle-and-select)

(global-set-key (kbd "C-S-l") 'xen-mark-lines)

(global-set-key (kbd "C-c b") 'browse-url-at-point)

;;; Aliases and advices
;; I'm grown up, I can manage using y/n for even destructive commands.
(defalias 'yes-or-no-p 'y-or-n-p)

(defadvice show-paren-function (after my-echo-paren-matching-line activate)
  "If a matching paren is off-screen, echo the matching line."
  (when (char-equal (char-syntax (char-before (point))) ?\))
    (let ((matching-text (blink-matching-open)))
      (when matching-text
        (message matching-text)))))


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
  :config (global-company-mode))

(use-package diff-hl
  :config (global-diff-hl-mode))

(use-package drag-stuff
  :diminish drag-stuff-mode
  :config (progn
            (setq drag-stuff-modifier '(meta shift))
            (drag-stuff-global-mode)))

(use-package drupal-mode
  :load-path "~/.emacs.d/drupal-mode/"
  :config (progn
            (bind-key "C-a" 'xen-drupal-mode-beginning-of-line drupal-mode-map)
            (add-hook 'drupal-mode-hook (lambda () (column-enforce-mode)))
            (delight 'drupal-mode
                     (propertize (concat " " [#xF1A9])
                                 'face '(:family "FontAwesome")))))

(use-package drush-make-mode)

(use-package ede-php-autoload-mode
  :commands ede-php-autoload-mode
  :load-path "~/.emacs.d/ede-php-autoload/"
  :init (add-hook 'php-mode-hook 'ede-php-autoload-mode))

(use-package eldoc
  :commands eldoc-mode
  :diminish "")

(use-package expand-region
  :bind ("C-S-SPC" . er/expand-region)
  :config (add-hook 'php-mode-hook 'xen-php-mode-expansions))

(use-package flycheck
  ;; Enable flycheck globally, doing it this way delays the setup to
  ;; after everything is loaded..
  :config (progn (add-hook 'after-init-hook #'global-flycheck-mode)))

(use-package flyspell
  :commands flyspell-mode
  :diminish "")

;; http://www.emacswiki.org/emacs/FrameMove
(use-package framemove
  :config (setq framemove-hook-into-windmove t))

(use-package geben
  :commands geben
  :load-path "~/.emacs.d/geben/")

(use-package google-this
  :diminish google-this-mode
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
  :config (progn
            (ivy-mode 1)
            (bind-key "C-S-s" 'ivy-yank-word ivy-mode-map))
  :bind (("C-s" . swiper)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> l" . counsel-load-library)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)
         ("C-c g" . counsel-git)
         ("C-c a" . counsel-ag)))

;; Properly handle annotations in java-mode.
(use-package java-mode-indent-annotations
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
  :init (setq magit-last-seen-setup-instructions "1.4.0")
  :config (add-hook 'magit-log-edit-mode-hook 'xen-magit-log-edit-mode-hook)
  (delight 'magit-status-mode (propertize (concat " " [#xF1D3])
                                  'face '(:family "FontAwesome")) :major))

;; Add git flow extension.
(use-package magit-gitflow
  :ensure t
  :commands turn-on-magit-gitflow
  :diminish magit-gitflow-mode
  :init
  (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))

;; Add github pull request extension.
;; (use-package magit-gh-pulls
;;   :commands turn-on-magit-gh-pulls
;;   :init (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))

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

(use-package navorski
  :commands nav/defterminal
  :init (nav/defterminal
          guard
          :program-path "/usr/local/bin/guard"
          :cwd '(lambda (path) (locate-dominating-file path "Guardfile"))
          :interactive t
          ))

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
             ;; Is this needed anymore?
             (modify-syntax-entry ?_ "_" php-mode-syntax-table)
             (yas-minor-mode 1)
             (ggtags-mode)
             )))

(use-package projectile
  :commands projectile-project-p
  :diminish projectile-mode
  :init (progn
    (projectile-global-mode)))

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
  :config (add-hook 'prog-mode-hook #'eldoc-mode))

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

(use-package string-inflection)

(use-package term
  :defer
  ;; Get paste working in (multi-)term-mode.
  :config (add-hook 'term-mode-hook
                    (lambda ()
                      (define-key term-raw-map (kbd "C-y") 'term-paste)
                      (define-key term-raw-map (kbd "C-v") 'xen-paste-term)
                      (define-key term-raw-map (kbd "S-C-v") 'xen-paste-term))))

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
  :config (progn (winner-mode)
                 (global-set-key [(XF86Back)] 'winner-undo)
                 (global-set-key [(XF86Forward)] 'winner-redo)))

(use-package ws-butler
  :commands ws-butler-mode
  :delight ws-butler-mode
  :init (add-hook 'php-mode-hook 'ws-butler-mode))

(use-package xen
  :load-path "~/.emacs.d/xen/"
  :demand
  :bind (("M-l" . xen-avy-goto-line)
         ;; Can we get 'n' to jump to $nid in php-mode? $ counts as
         ;; part of words in php-mode, that's the issue.
         ("C-<tab>" . xen-swiper)))

(use-package yaml-mode
  :config (flyspell-mode))

(use-package yasnippet
  :diminish yas-minor-mode
  :config (yas-reload-all))

(use-package zeal-at-point
  :bind (("C-c d" . zeal-at-point)))



;;; Some places for inspiration
;; https://github.com/geerds/emacs.d/blob/master/init.el
;; https://github.com/tomjakubowski/.emacs.d/blob/master/init.el
;; http://www.aaronbedra.com/emacs.d/

;; http://batsov.com/articles/2011/11/30/the-ultimate-collection-of-emacs-resources/

;;; Old comments left around...

; Try out http://www.emacswiki.org/emacs/MiniMap ?

; Solarized color scheme.
;(add-to-list 'load-path "~/.emacs.d/emacs-color-theme-solarized/")
;(require 'color-theme-solarized)


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
