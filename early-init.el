;;; early-init.el ---                                -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)
;; Let's go with without window titlebar. (At least while Gnome thinks
;; it should make them huuuge)
(add-to-list 'default-frame-alist '(undecorated . t))
;; Add a bit of spacing for pop-os/shells window highlight.
(add-to-list 'default-frame-alist '(internal-border-width . 3))

;; Setup some things that can't wait for customize to load. Set
;; foreground and background color to avoid flashing when doom-theme
;; gets activated.
(set-face-foreground 'default "#bbc2cf")
(set-face-background 'default "#21242b")
;; Set font too. TODO: This should be determined automatically:
;; https://emacs.stackexchange.com/questions/28390/quickly-adjusting-text-to-dpi-changes
(set-face-attribute 'default nil :height 120 :width
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

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
