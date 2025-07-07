;;; early-init.el --- Early initialization.  -*- lexical-binding: t; -*-

;;; Commentary:

;; Since Emacs 27.1, this file has been loaded before package
;; initialization and GUI. I use it to disable package.el, setup
;; Elpaca and Setup, and appearance stuff that's nice to have before
;; the first frame is displayed.

;;; Code:

(setopt
 ;; Disable package.el, I use Elpaca.
 package-enable-at-startup nil
 ;; Disable the mode-line in the "initial buffer" which is shown while
 ;; initializing. This prevents ugly old-school mode-line showing up
 ;; before doom-modeline has done its thing.
 mode-line-format nil
 ;; Relocate customs file so we don't clutter init.el with them.
 custom-file (locate-user-emacs-file "custom.el") )

;; Let's go with without window titlebar. (At least while Gnome thinks
;; it should make them huuuge)
(add-to-list 'default-frame-alist '(undecorated . t))
;; Add a bit of spacing for pop-os/shells window highlight.
(add-to-list 'default-frame-alist '(internal-border-width . 3))

;; Set foreground and background color to avoid flashing when
;; doom-theme gets activated. And set font too.
;; TODO: This could be determined automatically:
;; https://emacs.stackexchange.com/questions/28390/quickly-adjusting-text-to-dpi-changes
(custom-set-faces '(default
                    ;; Colors disabled for the moment as they override
                    ;; the latter themes.
                    ((t ( ;; :foreground "#3B4252"
                         ;; :background "#E5E9F0"
                         :height 120
                         :width semi-condensed
                         :foundry "FBI "
                         :family "Input")))
                    'now "Customized in early-init.el"))

;; Disable tool-bar, scroll-bars and menu-bar.
(dolist (disable-mode '(tool-bar-mode
                        menu-bar-mode
                        scroll-bar-mode))
  (when (fboundp disable-mode)
    (funcall disable-mode -1)))

;; Add lisp/ to load path so we can require files in there.
(push (locate-user-emacs-file "lisp") load-path)

(require '+elpaca)
(require '+setup)

(provide 'early-init)

;;; early-init.el ends here
