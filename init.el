;;; init.el --- Xens emacs configuration.  -*- flycheck-emacs-lisp-load-path: inherit; -*-
;;; Commentary:

;; My Emacs init.el.

;;; Code:
;; Setup some things that can't wait for customize to load. Set
;; foreground and background color to avoid flashing when doom-theme
;; gets activated.
(set-face-foreground 'default "#bbc2cf")
(set-face-background 'default "#21242b")
;; Set font too. TODO: This should be determined automatically:
;; https://emacs.stackexchange.com/questions/28390/quickly-adjusting-text-to-dpi-changes
(set-face-attribute 'default nil :height 110 :width
                    'semi-condensed :foundry "FBI " :family "Input")
;; Disable the mode-line in the "initial buffer" which is shown while
;; initializing. This prevents ugly old-school mode-line showing up
;; before doom-modeline has done its thing.
(setq mode-line-format nil)
;; Disable tool-bar.
(tool-bar-mode 0)
;; Disable scroll-bars.
(scroll-bar-mode 0)
;; Disable menu-bar.
(menu-bar-mode 0)

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
(setq custom-file (locate-user-emacs-file "custom.el"))

;;; Aliases and advices
;; I'm grown up, I can manage using y/n for even destructive commands.
(defalias 'yes-or-no-p 'y-or-n-p)



(defvar elpaca-installer-version 0.3)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (kill-buffer buffer)
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

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

(use-package use-package
  :config
  ;; Collect statistics for use-package-report.
  (setq use-package-compute-statistics t)
  :custom
  (use-package-verbose t))

;; Core emacs stuff. Some parts was nicked from https://github.com/grettke/lolsmacs
(use-package emacs
  :elpaca nil
  :delight
  (auto-fill-function)
  (abbrev-mode)
  :bind
  ;; Used to M-DEL deleting a word.
  ("M-<delete>" . kill-word)
  ;; Don't iconify on C-z.
  ("C-z" . nil)
  ;; Scrolling on C-v confuses me when my muscle memory tries to use it as paste.
  ("C-v" . nil)
  ;; Take out it's mate for consistency.
  ("M-v" . nil)
  ;; Quickly delete the current buffer.
  ("C-x C-k" . kill-current-buffer)
  ;; And often I want to kill the window too.
  ("C-x K" . kill-buffer-and-window)
  ;; Horizontal scrolling on trackpad produces these, which makes Emacs
  ;; print warnings about undefined keys. I don't want to do anything on
  ;; horizontal scroll.
  ("<mouse-6>" . ignore)
  ("<mouse-7>" . ignore)
  ;; Window resizing.
  ("S-C-<left>" . shrink-window-horizontally)
  ("S-C-<right>" . enlarge-window-horizontally)
  ("S-C-<down>" . shrink-window)
  ("S-C-<up>" . enlarge-window)
  ;; Quickly open URLs. Really obsoleted by embark, but for my muscle
  ;; memory.
  ("C-c b" . browse-url-at-point)
  ;; Alternatives: electric-buffer-list or bs-show.
  ("C-x C-b" . ibuffer)
  :hook (prog-mode . eldoc-mode)
  (prog-mode . (lambda () (setq-local comment-auto-fill-only-comments t)
                 (auto-fill-mode)))
  ;; Some variables set above is duplicated here to make use-package
  ;; hide them from re-exporting by custom. Alternatively I could
  ;; create a theme like use-package does and use
  ;; custom-theme-set-variables to set them.
  :custom
  (ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold] "Better ANSI colors")
  (auth-sources '("~/.config/emacs/authinfo.gpg") "Move auth-sources to XDG_CONFIG")
  (auto-hscroll-mode 'current-line "Only scroll current line")
  (backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
                          "Don't place backups next to the original file, but move them to <user-emacs-directory>/backups")
  (backward-delete-char-untabify-method 'all "Also delete newlines on backward-delete-char-untabify")
  (browse-url-browser-function 'browse-url-generic "Use browse-url-generic-program")
  (browse-url-generic-program "sensible-browser" "Set a working browser")
  (c-basic-offset 'set-from-style "Use indent from c-style")
  (c-default-style '((java-mode . "java") (awk-mode . "awk") (php-mod . "psr2") (other . "gnu")) "Set c-styles")
  ;; Lock files mess with watchers, and I don't have much use for it
  ;; on a single-user system.
  (create-lockfiles nil "Don't create lockfiles")
  (echo-keystrokes 0.02 "Echo keystrokes quickly")
  (ediff-split-window-function 'split-window-horizontally "Split windows horizontally")
  (ediff-window-setup-function 'ediff-setup-windows-plain "Use a single frame for all ediff windows")
  (enable-recursive-minibuffers t "Allow recursive minibuffers")
  (eval-expression-print-level nil "Print everything when eval'ing")
  (help-window-select t "Makes it easier to dismiss them with q")
  (history-delete-duplicates t "No need for dupes in history")
  (hscroll-margin 15 "Increase margin for horizontal scroll")
  (indent-tabs-mode nil "Don't use tabs for indentation")
  (inhibit-startup-screen t "Don't need the startup screen anymore")
  (isearch-lazy-count t "Show counts in isearch")
  (jit-lock-stealth-time 10 "Seconds idle before starting to fontify in the background")
  (js-indent-level 2 "Set indent level")
  (lazy-count-prefix-format nil "Counts before the seach string messes with readability...")
  (lazy-count-suffix-format " [%s of %s]" "...so show them as suffix.")
  (line-move-visual nil "Don't move by visual lines")
  (menu-bar-mode nil "Remove menu bar")
  (mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))) "Make scrollwheel behave more like in other apps")
  (mouse-yank-at-point t "Yank at point, not pointer position when mouse-yanking")
  (load-prefer-newer t "Prefer newer .el file over .elc")
  (max-mini-window-height 0.33 "Give mini-buffers a bit more room")
  (password-cache-expiry 3600 "Cache passwords a bit longer")
  (query-replace-highlight t "Highlight matches when query-replacing")
  (recentf-max-saved-items 500 "Save more items in recent files")
  (safe-local-variable-values '((flycheck-emacs-lisp-load-path . inherit)) "Allow inherit for flycheck-emacs-lisp-load-path for my init.el.")
  (save-interprogram-paste-before-kill t "Don't lose clips from other programs")
  (scroll-bar-mode nil "Don't show scrollbars")
  (scroll-conservatively 2 "Scroll linewise rather than jumping")
  (scroll-margin 5 "Keep a margin to top/bottom of window")
  (scroll-preserve-screen-position t "Don't jump around when scrolling")
  (sentence-end-double-space nil "Don't require double space after period to consider it a sentence")
  (set-mark-command-repeat-pop t "Allow for repeatedly popping the mark using C-SPC")
  (shift-select-mode nil "Don't use shift + cursors to mark regions")
  (track-eol t "Want to stick to end of line")
  (uniquify-after-kill-buffer-p t "Re-uniquify buffers after killing some")
  (uniquify-buffer-name-style 'post-forward-angle-brackets "Use appended brackets for file path")
  (uniquify-trailing-separator-p t "Add a slash to directory buffers")
  (url-cookie-confirmation 'nil "Don't require confirmation on cookies")
  (use-dialog-box nil "Disable (mouse) dialogs, something is confusing emacs making it think some commands were mouse initiated")
  (user-mail-address "xen@xen.dk" "Set email address")
  (warning-minimum-level :error "Only show warnings buffer on errors")
  (wdired-allow-to-change-permissions t "Allow C-x C-q to change permissions too")
  (whitespace-style '(face tabs tab-mark) "Make tabs more visible")
  :init
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



;;; Packages.

;; Reinstall these when the need arise:
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
  ;; Make the comments brighter and loose the background color.
  (setq doom-one-brighter-comments t)

  ;; Load the theme
  (load-theme 'doom-one t)

  ;; Make the background darker.
  (set-face-background 'default "#21242b")

  ;; Remove background color on comments.
  (set-face-background 'font-lock-comment-face nil)

  ;; Adjust hl-line color.
  (with-eval-after-load 'hl-line
    (set-face-background 'hl-line "#282c34"))

  ;; Make symbol highlight and region highlights a darker version of the region.
  (with-eval-after-load 'region-occurrences-highlighter
    (set-face-inverse-video 'region-occurrences-highlighter-face nil)
    (set-face-background 'region-occurrences-highlighter-face (doom-blend (doom-color 'region) (doom-color 'bg) 0.50)))
  (with-eval-after-load 'highlight-symbol
    (set-face-background 'highlight-symbol-face (doom-blend (doom-color 'region) (doom-color 'bg) 0.50)))

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  ;; Load after flycheck-color-mode-line in order for doom-modeline to
  ;; remaps its mode-line faces.
  :after (flycheck-color-mode-line)
  :init
  (setq doom-modeline-buffer-file-name-style 'truncate-except-project)
  (doom-modeline-mode)
  :config
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
  (with-eval-after-load "seq"
    (let ((buffers (seq-filter
                    (lambda (buffer)
                      (with-current-buffer buffer
                        (derived-mode-p 'special-mode)))
                    (buffer-list))))
      (dolist (buffer buffers)
        (with-current-buffer buffer
          (doom-modeline-set-modeline 'xen-minimal)))))
  ;; Use xen-minimal in misc virtual buffers.
  :hook ((vterm-mode
          lisp-interaction-mode
          ;; special-mode covers messages buffer, dashboard, help
          ;; buffer and more.
          special-mode) . (lambda ()
                            ;; dashboard calls its hooks before
                            ;; doom-modeline is loaded, so guard the
                            ;; set with boundp. The dashboard buffer
                            ;; modeline will be fixed by the
                            ;; with-eval-after-load.
                            (if (fboundp 'doom-modeline-set-modeline)
                                (doom-modeline-set-modeline 'xen-minimal)))))

(use-package aas
  :hook (php-mode . aas-activate-for-major-mode)
  :config
  (aas-set-snippets 'php-mode
    :cond (lambda ()
            (not (xen-in-comment)))
    "if " '(tempel "if (" (p "condition") ") {" n> r ";" n> "}")
    "else " '(tempel "else {" n> r ";" n> "}")
    "try " '(tempel "try {" n> p n> "} catch (" (p "\\Throwable") " $o_O) {" n> r n> "}")
    "fore" '(tempel "foreach (" (p "array") " as $" (p "value") ") {" n> r ";" n> "}")
    "$" (lambda () (interactive)
          (if (looking-back "as \\$")
              (tempel-insert '((p "key") " => $" (p "value")))
            (insert "$")))
    :cond #'bolp
    "dst" "declare(strict_types=1);\n"))

(use-package all-the-icons
  :config
  (add-to-list 'all-the-icons-mode-icon-alist '(vterm-mode all-the-icons-octicon "terminal" :v-adjust 0.2 :height 1.0)))

;; Make sure that delight is available as soon as any package triggers it.
(use-package delight
  :commands delight)

(use-package apib-mode
  :defer t
  :mode "\\.apib$")

(use-package avy
  :custom
  (avy-background t)
  (avy-keys '(97 101 117 105 100 104 116 115))
  (avy-style 'de-bruijn)
  ;; Override minor mode binding for these.
  :bind*
  ;; Binding xen-avy-goto-word-1 and xen-avy-goto-line on
  ;; use-package xen.
  ("M-u" . avy-goto-char-in-line)
  ("M-U" . avy-goto-char))

(use-package avy-zap
  :bind
  ("M-z" . avy-zap-to-char-dwim)
  ("M-Z" . avy-zap-up-to-char-dwim))

;; Built in, used by magit, we'll delight it.
(use-package autorevert
  :elpaca nil
  :commands auto-revert-mode
  :delight auto-revert-mode)

(use-package bug-hunter
  :commands (bug-hunter-init-file bug-hunter-file))

(use-package buttercup
  :defer t)

(use-package caddyfile-mode
  :mode (("Caddyfile\\'" . caddyfile-mode)
         ("caddy\\.conf\\'" . caddyfile-mode)))

(use-package cask-mode
  :defer t)

(use-package css-mode
  :elpaca nil
  :custom
  (css-fontify-colors nil "Use rainbow-mode hacked to use overlays so it works nicely with hl-line")
  (css-indent-offset 2 "Set default CSS indent offset")
  :commands css-mode)

(use-package column-enforce-mode
  :commands column-enforce-mode
  :delight
  :hook drupal-mode
  :config
  (with-eval-after-load "doom-themes"
    (set-face-attribute 'column-enforce-face nil :inherit nil
                        :underline nil
                        :background (doom-darken 'warning .75))))

(use-package company
  :commands global-company-mode
  :delight
  ;; Global mode, can't really be deferred, but delay it until
  ;; php-extras and xen-company has had a chance to define their
  ;; completers and functions.
  :after (php-extras xen-company)
  :defines company-semantic-modes
  :custom
  (company-auto-commit nil)
  (company-backends
   (quote
    (company-elisp
     (:separate company-capf php-extras-company company-dabbrev-code-xen company-gtags company-keywords :with company-yasnippet)
     company-bbdb company-semantic company-clang company-cmake
     (company-dabbrev-code-xen company-gtags company-etags company-keywords)
     company-oddmuse company-files company-dabbrev)))
  (company-dabbrev-code-everywhere nil)
  (company-idle-delay 0)
  (company-minimum-prefix-length 2)
  (company-require-match nil)
  (company-search-regexp-function (quote company-search-words-in-any-order-regexp))
  (company-show-numbers nil)
  (company-tooltip-align-annotations t)
  (company-tooltip-flip-when-above nil)
  (company-tooltip-idle-delay 0)
  (company-tooltip-limit 30)
  (company-tooltip-minimum 20)
  (company-transformers (quote (company-sort-by-occurrence xen-company-filter)))
  :config
  ;; Use the TAB only frontend. Configure before enabling the mode, so
  ;; we'll get the tng frontend in before anyone makes
  ;; company-frontends buffer local.
  (company-tng-mode)

  ;; Define a frontend that displays a preview, but only when tng
  ;; hasn't made a selection yet.
  (defun company-preview-common-if-not-tng-frontend (command)
    "`company-preview-frontend', but not when tng is active."
    (unless (or (and (eq command 'post-command)
                     company-selection-changed
                     (memq 'company-tng-frontend company-frontends))
                ;; company-preview-common-frontend does not like when
                ;; company-common is an empty string (rather than nil)
                ;; in 'post-command.
                (and (eq command 'post-command)
                     (equal company-common "")))
      (company-preview-common-frontend command)))
  ;; Variant of `company-pseudo-tooltip-unless-just-one-frontend' that
  ;; still shows a dropdown with only one candidate when filtering.
  (defun company-pseudo-tooltip-unless-just-one-frontend-2 (command)
    "`company-pseudo-tooltip-frontend', but not shown for single
candidates, unless we're in filtering mode."
    (unless (and (eq command 'post-command)
                 (and (company--show-inline-p) (not company-search-filtering)))
      (company-pseudo-tooltip-frontend command)))

  (setq company-frontends '(company-tng-frontend
                            company-pseudo-tooltip-unless-just-one-frontend-2
                            company-preview-common-if-not-tng-frontend
                            company-echo-metadata-frontend))

  (global-company-mode)
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

  ;; Swap search and filter shortcuts. I prefer filtering the dropdown
  ;; rather than searching in it.
  (define-key company-active-map "\C-s" 'company-filter-candidates)
  (define-key company-active-map "\C-\M-s" 'company-search-candidates))

(use-package company-restclient
  :after (company restclient))

(use-package company-tabnine
  :disabled
  :after company
  :custom
  (company-tabnine-binaries-folder "~/.config/emacs/tabnine" "Point to binary"))

(use-package copilot
  :elpaca (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :disabled t
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

  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root))

(use-package consult-flycheck)

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
  ;; The language server can't figure out how to indent regions, so
  ;; it's basically useless. Tell lsp-mode not to use it.
  :hook (crystal-mode . (lambda ()
                          (setq-local lsp-enable-indentation nil)))
  :bind (:map crystal-mode-map
              ("C-c C-t" . crystal-spec-switch)))

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

(use-package dashboard
  :commands dashboard-setup-startup-hook
  :demand
  ;; Need page-break-lines for pretty separators.
  :after page-break-lines
  :defines (dashboard-startup-banner dashboard-item-generators dashboard-items)
  :init
  ;; Elpaca triggers in after-init-hook, so dashboard gets in too
  ;; late. Copy it's hooks to elpacas hook.
  (add-hook 'elpaca-after-init-hook (lambda ()
                                      ;; Display useful lists of items
                                      (dashboard-insert-startupify-lists)))
  (add-hook 'elpaca-after-init-hook (lambda ()
                                      (switch-to-buffer dashboard-buffer-name)
                                      (goto-char (point-min))
                                      (redisplay)
                                      (run-hooks 'dashboard-after-initialize-hook)))
  :custom
  (dashboard-page-separator "\n\f\n" "Use page-break-lines-mode")
  :config
  (setq dashboard-startup-banner 'logo)
  (add-to-list 'dashboard-item-generators '(xen-tip . xen-dashboard-tip))
  (setq dashboard-items '((projects . 10) (xen-tip)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t))

;; Built in.
(use-package delsel
  :elpaca nil
  :init
  (delete-selection-mode))

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

(use-package diff-hl
  :commands global-diff-hl-mode
  :init (global-diff-hl-mode))

(use-package dimmer
  :init
  (dimmer-configure-which-key)
  (dimmer-configure-magit)
  (dimmer-mode t))

;; Built in.
(use-package display-line-numbers-mode
  :elpaca nil
  :commands display-line-numbers-mode
  :custom
  (display-line-numbers-grow-only t "Only grow room for line numbers")
  (display-line-numbers-major-tick 20 "Show major line ever 20 lines")
  (display-line-numbers-minor-tick 5 "Show minor line every 5 lines")
  :custom-face
  (line-number-major-tick ((t (:foreground "dark gray"))))
  (line-number-minor-tick ((t (:foreground "dim gray"))))
  :hook (prog-mode . (lambda ()
                       "Enable line numbers in file-visiting buffers."
                       (when (buffer-file-name (buffer-base-buffer))
                         (display-line-numbers-mode 1)))))

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

(use-package eldoc
  :commands eldoc-mode
  :delight)

(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-)" . embark-act)
   ("C-," . embark-dwim)
   ("C-(" . embark-dwim)
   ("C-h B" . embark-bindings)
   :map embark-region-map
   ("s" . sort-lines)
   ("u" . delete-duplicate-lines)
   ("/" . xen-google-region)
   ("C-s" . xen-region-isearch-forward)
   ("C-r" . xen-region-isearch-backward))
  :custom
  (embark-verbose-indicator-display-action
   '(display-buffer-at-bottom (window-height . fit-window-to-buffer))
   "Make embark-verbose-indicator shrink to content")
  :config
  ;; Use which-key as indicator, rather than the minimal default or
  ;; the completing-read one.
  ;;   (defun embark-which-key-indicator (keymap targets)
  ;;     "An embark indicator that displays KEYMAP with which-key.
  ;; The which-key help message will show the type and value of the
  ;; current target followed by an ellipsis if there are further
  ;; TARGETS."
  ;;   (which-key--show-keymap
  ;;    (if (eq (caar targets) 'embark-become)
  ;;        "Become"
  ;;      (format "Act on %s '%s'%s"
  ;;              (caar targets)
  ;;              (embark--truncate-target (cdar targets))
  ;;              (if (cdr targets) "…" "")))
  ;;    keymap
  ;;    nil nil t)
  ;;   (lambda (prefix)
  ;;     (if prefix
  ;;         (embark-which-key-indicator (lookup-key keymap prefix) targets)
  ;;       (kill-buffer which-key--buffer))))

  ;;   (setq embark-indicator #'embark-which-key-indicator)
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

(use-package expand-region
  :bind ("C-S-SPC" . er/expand-region)
  :custom
  (expand-region-subword-enabled t "Use subword expansion")
  :demand t)

(use-package feature-mode
  ;; See readme for how to set up jump to step.
  :defer t)

(use-package fish-mode
  :defer t)

(use-package flycheck
  :custom
  (flycheck-disabled-checkers (quote (javascript-jshint)))
  (flycheck-eslintrc nil)
  (flycheck-global-modes (quote (not org-mode vterm-mode)))
  (flycheck-javascript-eslint-executable "/home/xen/.npm-global/bin/eslint")
  (flycheck-mode-line (quote (:eval (xen-flycheck-mode-line-status-text))))
  (flycheck-phpmd-rulesets (quote ("codesize" "design" "naming")))
  (flycheck-scss-compass t)
  :bind (:map flycheck-mode-map
              ("M-<up>" . flycheck-previous-error)
              ("M-<down>" . flycheck-next-error))
  ;; Enable flycheck globally, doing it this way delays the setup to
  ;; after everything is loaded.
  :hook (after-init . global-flycheck-mode))

(use-package flycheck-cask
  :commands flycheck-cask-setup
  :hook (flycheck-mode . flycheck-cask-setup))

(use-package flycheck-color-mode-line
  :commands flycheck-color-mode-line-mode
  ;; doom-modeline depends on this, so it can't be deferred, lest we
  ;; want to see default modelines.
  :demand t
  :hook (flycheck-mode . flycheck-color-mode-line-mode)
  :config
  (with-eval-after-load "doom-themes"
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

(use-package flycheck-package
  :commands flycheck-package-setup
  :hook (flycheck-mode . flycheck-package-setup))

(use-package flycheck-phpstan
  :hook
  (php-mode . (lambda ()
                ;; Use error level from phpstan.neon.
                (setq phpstan-level nil)))
  (lsp-diagnostics-mode . (lambda () (flycheck-add-next-checker 'php-phpcs 'phpstan)))
  :after (flycheck lsp-mode)
  :custom
  (phpstan-enable-on-no-config-file nil))

(use-package flyspell
  :elpaca nil
  :commands flyspell-mode
  :custom
  (flyspell-default-dictionary nil)
  :hook
  ((gfm-mode yaml-mode org-mode) . flyspell-mode)
  ((emacs-lisp-mode php-mode css-mode js-mode enh-ruby-mode) . flyspell-prog-mode)
  (git-commit-mode . flyspell-mode)
  :config
  ;; Want to hand these over to embark.
  (unbind-key "C-." flyspell-mode-map)
  (unbind-key "C-," flyspell-mode-map)
  :delight)

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

(use-package forge
  :after magit)

;; http://www.emacswiki.org/emacs/FrameMove
(use-package framemove
  ;; Elpaca doesn't have a menu for emacsmirror yet.
  :elpaca (framemove :host github :repo "emacsmirror/framemove")
  :config (setq framemove-hook-into-windmove t))

(use-package git-attr
  :elpaca (:host github :repo "arnested/emacs-git-attr"))

(use-package go-mode
  :mode "\\.go\\'"
  :defer t
  :config
  ;; Tell gopls that we're using go modules.
  (setenv "GO111MODULE" "on")
  :hook ((go-mode . subword-mode)
         (go-mode . (lambda ()
                      (add-hook 'before-save-hook #'lsp-format-buffer t t)
                      (add-hook 'before-save-hook #'lsp-organize-imports t t)))))

(use-package google-this
  :commands (google-this-mode google-this-region)
  :delight
  ;; Why does this use use-package-autoload-keymap?
  :bind-keymap ("C-c /" . google-this-mode-submap)
  :config (google-this-mode))

(use-package github-review
  :after forge)

(use-package highlight-symbol
  :commands highlight-symbol-mode
  :custom
  (highlight-symbol-idle-delay 0.5)
  :delight
  :hook ((emacs-lisp-mode php-mode css-mode js-mode enh-ruby-mode) . highlight-symbol-mode)
  :bind
  ("M-<left>" . highlight-symbol-prev)
  ("M-<right>" . highlight-symbol-next)
  :config
  (add-hook 'activate-mark-hook (lambda () (when highlight-symbol-mode
                                             (setq-local highlight-symbol-mode-suspend t)
                                             (highlight-symbol-mode 0))))
  (add-hook 'deactivate-mark-hook (lambda () (when (bound-and-true-p highlight-symbol-mode-suspend)
                                               (kill-local-variable highlight-symbol-mode-suspend)
                                               (highlight-symbol-mode 1)))))

(use-package hl-line
  :elpaca nil
  ;; Let xen-vterm handle hl-line-mode toggling in vterm buffers.
  :hook
  (after-change-major-mode . (lambda ()
                               "Enable hl-line-mode unless in minibuffer or vterm-mode"
                               (unless (or (minibufferp)
                                           (eq major-mode 'vterm-mode))
                                 (hl-line-mode)))))

(use-package hungry-delete
  :delight
  :hook ((emacs-lisp-mode php-mode css-mode js-mode enh-ruby-mode crystal-mode) . hungry-delete-mode))

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

;; Standard Emacs package. Dead keys work when this is loaded.
(use-package iso-transl
  :elpaca nil)

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

(use-package lsp-mode
  :commands lsp lsp-deferred
  ;; Can't add to company-backends before company has been loaded.
  :after company
  :custom
  (lsp-keymap-prefix "C-c l" "Set the keymap prefix")
  (lsp-log-max 1000 "Limit log entries to a thousand lines")
  (lsp-serenata-file-extensions ["php" "inc" "module" "install" "theme"] "Add Drupal file extensions to scanned files")
  (lsp-serenata-index-database-uri "/home/xen/.cache/index.sqlite" "Set db path")
  (lsp-serenata-server-path "serenata" "Set server path")
  (lsp-solargraph-use-bundler t "Use the bundler installed Solargraph")
  (lsp-clients-crystal-executable '("crystalline" "--stdio") "Use crystalline instead of scry")
  :init
  ;; Recommended setup.
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-prefer-capf t)
  :hook
  (lsp-mode . lsp-ui-mode)
  (lsp-mode . lsp-enable-which-key-integration)
  ;; Check lsp-language-id-configuration for supported modes.
  ((css-mode
    crystal-mode
    dockerfile-mode
    go-mode
    html-mode
    js-mode
    json-mode
    ;; Doesn't work for gfm-mode. Also, install unified-language-server.
    ;; markdown-mode
    nxml-mode
    php-mode
    rjsx-mode
    enh-ruby-mode
    scss-mode
    sh-mode
    sql-mode
    typescript-mode
    xml-mode
    yaml-mode) . lsp-deferred)
  ;; This is not proper as the lsp checker is global across modes, and
  ;; this adds php-phpcs in all modes. But until lsp/flycheck deals
  ;; with this problem, this will work.
  (lsp-diagnostics-mode . (lambda () (flycheck-add-next-checker 'lsp 'php-phpcs)))
  :config
  (unbind-key "C-S-SPC" lsp-mode-map))

(use-package lsp-ui
  :commands (lsp-ui-mode lsp-ui-sideline-mode)
  :after lsp
  :custom-face
  (lsp-ui-sideline-global ((t (:background "#3f444a"))))
  :config
  ;; Use sideline mode in all flycheck buffers. Better than displaying
  ;; in mini-buffer or flycheck-inline.
  :hook (flycheck-mode . lsp-ui-sideline-mode))

(use-package nerd-icons)

(use-package nginx-mode
  :commands nginx-mode)

(use-package magit
  :defines magit-last-seen-setup-instructions
  :custom
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
  :bind (("C-c g g" ("Status" . magit-status))
         ("C-c g d" ("Dispatch" . magit-dispatch))
         ("C-c g f" ("File dispatch" . magit-file-dispatch)))
  :hook
  (git-commit-setup . xen-git-commit-setup)
  (git-commit-mode . turn-on-auto-fill)
  (git-commit-mode . git-commit-save-message)
  :config
  ;; Delight has better handling for major-modes.
  (delight 'magit-status-mode
           (propertize (concat " " [#xF1D3])
                       'face '(:family "FontAwesome")) :major))

;; Why this isn't handled by dependencies, I don't know.
(use-package magit-section)

;; Add git flow extension.
(use-package magit-gitflow
  :after magit
  :delight
  :hook (magit-mode . turn-on-magit-gitflow))

(use-package magit-filenotify
  :defer t)

(use-package magithub
  :commands magithub-feature-autoinject
  :disabled t
  :after magit
  :config (magithub-feature-autoinject t))

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

(use-package move-text
  :bind
  ("C-M-<up>" . move-text-up)
  ("C-M-<down>" . move-text-down))

(use-package mwim
  :bind
  ("C-a" . mwim-beginning)
  ("C-e" . mwim-end))

(use-package multi-line
  :bind
  ("C-c d" . multi-line))

;; From http://www.gerd-neugebauer.de/software/emacs/multi-mode/multi-mode.el
(use-package multi-mode
  :load-path "lib/"
  :commands multi-mode)
;; Example:
;; (multi-mode 1 'html-mode '("<?php" php-mode) '("?>" html-mode))

(use-package multiple-cursors
  :bind
  ("C-<" . mc/mark-previous-like-this)
  ("C->" . mc/mark-next-like-this)
  ("C-M-m" . mc/mark-more-like-this-extended)
  ("C-*" . mc/mark-all-like-this)
  ("C-%" . mc/mark-all-in-region)
  ("C-=" . mc/mark-all-like-this-dwim))

(use-package olivetti
  :defer t)

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

(use-package page-break-lines
  :delight
  ;; We need it before after-init-hook is triggered as dashboard depends on it.
  :demand
  :hook (emacs-lisp-mode . page-break-lines-mode))

(use-package php-boris
  :commands php-boris)

(use-package php-mode
  :commands php-mode
  :mode
  ("\\.php\\'" . php-mode)
  ("\\.inc\\'" . php-mode)
  ("\\.module\\'" . php-mode)
  :magic ("<?php" . php-mode-maybe)
  :custom
  (php-mode-coding-style (quote psr2))
  (php-mode-enable-project-coding-style nil)
  :config
  (require 'dap-php)
  :hook (php-mode . (lambda () (subword-mode 1))))

(use-package php-extras
  :custom
  (php-extras-auto-complete-insert-parenthesis nil)
  :elpaca (:host github :repo "arnested/php-extras"))

(use-package po-mode
  :defer t)

(use-package projectile
  :commands (projectile-mode projectile-project-p)
  :custom
  (projectile-cache-file (concat user-emacs-directory ".projectile.cache"))
  (projectile-known-projects-file (concat user-emacs-directory ".projectile-bookmarks.eld"))
  (projectile-switch-project-action (quote projectile-vc))
  :delight ""
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (projectile-mode)
  :config
  (projectile-register-project-type 'php-laravel '("composer.json" "app" "vendor")
                                    :compile "./artisan serve"
                                    :test "./vendor/bin/phpunit "
                                    :test-dir "tests"
                                    :test-suffix "Test"))

(use-package rainbow-mode
  :defer t
  :config
  ;; Use overlay instead of text properties to override `hl-line' faces.
  ;; Taken from https://github.com/seagle0128/.emacs.d/blob/bf3d66248b883638d0b7ab584d5074f019fb555d/lisp/init-highlight.el#L148
  (defun xen-rainbow-colorize-match (color &optional match)
    (let* ((match (or match 0))
           (ov (make-overlay (match-beginning match) (match-end match))))
      (overlay-put ov 'ovrainbow t)
      (overlay-put ov 'face `((:foreground ,(if (> 0.5 (rainbow-x-color-luminance color))
                                                "white" "black"))
                              (:background ,color)))))
  (advice-add #'rainbow-colorize-match :override #'xen-rainbow-colorize-match)

  (defun xen-rainbow-clear-overlays ()
    "Clear all rainbow overlays."
    (remove-overlays (point-min) (point-max) 'ovrainbow t))
  (advice-add #'rainbow-turn-off :after #'xen-rainbow-clear-overlays)
  :hook
  (css-mode . rainbow-mode))

(use-package reaper
  :load-path "reaper"
  :config
  (load (locate-user-emacs-file "reaper-key.el") :noerror :nomessage)
  ;; Store the autofile function in an uncommitted file.
  (load (locate-user-emacs-file "xen-reaper.el") :noerror :nomessage)
  :init
  (add-hook 'reaper-autofile-functions 'xen-reaper-autofile-function)
  :bind ("C-c h" . reaper))

(use-package region-occurrences-highlighter
  :hook ((emacs-lisp-mode php-mode css-mode js-mode enh-ruby-mode) . region-occurrences-highlighter-mode)
  :bind (:map region-occurrences-highlighter-nav-mode-map
              ("M-<up>" . region-occurrences-highlighter-prev)
              ("M-<down>" . region-occurrences-highlighter-next)))

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

;; Built in.
(use-package savehist
  :elpaca nil
  :custom
  (savehist-save-minibuffer-history t "Save mini-buffer history")
  (savehist-additional-variables
   '(kill-ring
     search-ring
     regexp-search-ring
     last-kbd-macro
     kmacro-ring
     shell-command-history) "Other interesting things to save")
  :init
  (savehist-mode))

;; Built in, but we need to activate it.
(use-package saveplace
  :elpaca nil
  :custom
  (save-place-file (concat user-emacs-directory "saveplaces"))
  :init
  (save-place-mode))

(use-package smartparens
  :commands (smartparens-mode smartparens-global-mode show-smartparens-global-mode sp-pair sp-local-pair sp-with-modes)
  :custom
  (sp-autodelete-closing-pair nil)
  (sp-autodelete-opening-pair nil)
  (sp-autodelete-pair nil)
  (sp-autoskip-closing-pair (quote always-end) "Only skip typed closing pair when we're at it")
  (sp-show-pair-from-inside t)
  :demand
  :delight
  :config
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
  )

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

;; Used through aas. Couldn't get corfu working.
(use-package tempel
  :bind (:map tempel-map
              ("<tab>" . tempel-next)))

(use-package twig-mode
  :defer t)

(use-package undo-tree
  :commands global-undo-tree-mode
  :custom
  (undo-tree-auto-save-history t)
  (undo-tree-history-directory-alist (list (cons "." (concat user-emacs-directory "undo-history"))))
  (undo-tree-visualizer-diff nil)
  (undo-tree-visualizer-relative-timestamps t)
  (undo-tree-visualizer-timestamps t)
  (undo-tree-incompatible-major-modes '(term-mode special-mode) "Don't use undo-tree in special-mode buffers")
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
  :elpaca (:type git :host gitlab :repo "tsc25/undo-tree"))

(use-package vcl-mode
  :commands vcl-mode
  :mode "\\.vcl\\'"
  :custom
  (vcl-indent-level 2 "Set indent level"))

(use-package watch-buffer
  :commands watch-buffer)

(use-package which-key
  :init
  (which-key-mode)
  :custom
  (which-key-idle-secondary-delay 0.1)
  (which-key-mode t)
  (which-key-popup-type (quote side-window))
  (which-key-show-early-on-C-h t)
  (which-key-side-window-max-height 0.5)
  (which-key-sort-order (quote which-key-key-order)))

(use-package vertico
  :elpaca (:files (:defaults "extensions/*"))
  :bind (:map vertico-map
              ("S-SPC" . vertico-quick-exit))
  :custom
  (vertico-quick1 "aoeu" "Set quick navigation keys")
  (vertico-quick2 "dhtn" "Ditto")
  :init
  (vertico-mode))

(use-package vertico-prescient
  :init
  ;; todo: is this customs?
  (setq prescient-filter-method '(literal initialism prefix regexp)
        prescient-sort-full-matches-first t)
  (vertico-prescient-mode 1)
  (prescient-persist-mode 1))

(use-package visual-fill-column
  :commands visual-fill-column-mode
  :custom
  (visual-fill-column-center-text t "Center text when using this mode"))

(use-package visual-regexp
  :bind
  ("C-c r" . vr/replace)
  ("C-c q" . vr/query-replace)
  ("C-c m" . vr/mc-mark)
  :custom
  (vr/default-replace-preview t "Show preview"))

(use-package vterm
  :custom
  (vterm-max-scrollback 100000)
  (vterm-buffer-name-string "vterm: %s")
  :bind (:map vterm-mode-map
              ;; Fish understands C-g too.
              ("C-g" . vterm--self-insert)
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

;; http://www.emacswiki.org/emacs/WindMove
;; Built in.
(use-package windmove
  :elpaca nil
  :init
  ;; Shift is default.
  (windmove-default-keybindings)
  (windmove-swap-states-default-keybindings '(shift meta))
  ;; Let's try this on for size.
  (windmove-display-default-keybindings '(shift meta ctrl))
  ;; Make windmove work in org-mode:
  :hook
  (org-shiftup-final  . windmove-up)
  (org-shiftleft-final  . windmove-left)
  (org-shiftdown-final  . windmove-down)
  (org-shiftright-final  . windmove-right))

;; http://www.emacswiki.org/emacs/WinnerMode
;; Built in.
(use-package winner
  :elpaca nil
  :config (winner-mode))

(use-package ws-butler
  :commands ws-butler-mode
  :delight
  :hook ((emacs-lisp-mode php-mode enh-ruby-mode css-mode js-mode feature-mode) . ws-butler-mode))

(use-package xen
  :elpaca nil
  :load-path "xen"
  :commands (xen-region-isearch-forward xen-region-isearch-backward)
  :demand
  ;;:functions xen-coding-common-bindings
  :hook
  ((emacs-lisp-mode php-mode css-mode js-mode enh-ruby-mode) . xen-coding-common-bindings)
  :bind* ("S-SPC" . xen-avy-goto-word-1)
  :bind (("RET" . xen-newline)
         ("M-SPC" . xen-cycle-spacing)
         ("M-l" . xen-avy-goto-line)
         ("<f12>" . olivetti-mode)
         ("C-S-d" . xen-duplicate-current-line)
         ("C-S-l" . xen-mark-lines)
         ("C-c x" . xen-map)
         ("M-g g" . xen-avy-goto-line)
         ("M-g M-g" . xen-avy-goto-line)
         ("M-c" . xen-casing-map)
         ("C-c y" . xen-edit-clipboard)
         :map xen-casing-map
         ("c" ("Capitalize" . capitalize-word))
         ("u" ("UPPERCASE" . upcase-word))
         ("l" ("downcase" . downcase-word))
         ("s" ("snake_case" . string-inflection-underscore))
         ("n" ("UPPER_SNAKE" . string-inflection-upcase))
         ("a" ("camelCase" . string-inflection-lower-camelcase))
         ("m" ("CamelCase" . string-inflection-camelcase))
         ("k" ("kebab-case" . string-inflection-kebab-case))))

(elpaca nil
  (use-package xen-company
    :elpaca nil
    :load-path "xen")

  (use-package xen-flycheck
    :elpaca nil
    :load-path "xen"
    :functions xen-flycheck-mode-line-status-text
    :bind (:map flycheck-mode-map
                ("C-c ! s" . xen-flycheck-insert-suppressor)))

  (use-package xen-paired-delete
    :elpaca nil
    :load-path "xen"
    :after (smartparens)
    :config
    (global-xen-paired-delete-mode))

  (use-package xen-php
    :elpaca nil
    :load-path "xen"
    :commands xen-php-setup-tools
    :hook
    ;; Use hack-local-variables-hook to run after `.dir-local.el'
    ;; variables has been set.
    (php-mode . (lambda ()
                  (add-hook 'hack-local-variables-hook #'xen-php-setup-tools nil t)))
    :config
    (with-eval-after-load "expand-region"
      (add-hook 'php-mode-hook #'xen-php-mode-expansions))
    (sp-with-modes '(php-mode)
      (sp-local-pair "/*" "*/" :post-handlers '(("| " "SPC")
                                                ("* |\n[i]" "RET")
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

  (use-package xen-projectile
    :elpaca nil
    :load-path "xen"
    :after (projectile)
    :bind (:map projectile-command-map
                ("s" . xen-projectile-switch-to-shell)
                ("S" . projectile-run-vterm)))

  (use-package xen-vterm
    :elpaca nil
    :load-path "xen"
    :hook (vterm-copy-mode . xen-vterm-copy-mode-hook)
    :bind
    ("C-c s" . xen-switch-to-shell)
    ("C-c S" . vterm)))

(use-package yaml-mode
  :mode "\\.(e?ya?ml|neon)\\(.dist\\)$")

(use-package yasnippet
  :commands yas-reload-all
  :custom
  (yas-choose-keys-first nil)
  (yas-choose-tables-first t)
  (yas-fallback-behavior (quote call-other-command))
  (yas-prompt-functions
   (quote
    (yas-dropdown-prompt yas-completing-prompt yas-ido-prompt yas-no-prompt)))
  (yas-triggers-in-field t)
  (yas-wrap-around-region nil)
  :delight yas-minor-mode
  :hook
  ((emacs-lisp-mode php-mode css-mode js-mode enh-ruby-mode) . yas-minor-mode)
  (git-commit-mode . yas-minor-mode)
  :config (yas-reload-all))



(load custom-file)

;; Enable some disabled commands.
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;; Some places for inspiration

;; https://github.com/tomjakubowski/.emacs.d/blob/master/init.el
;; http://www.aaronbedra.com/emacs.d/
;; http://batsov.com/articles/2011/11/30/the-ultimate-collection-of-emacs-resources/
;; https://www.youtube.com/watch?v=p3Te_a-AGqM&feature=player_embedded#!
;; http://emacsrocks.com/

(provide 'init)
;;; init.el ends here
