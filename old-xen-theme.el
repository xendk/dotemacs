(deftheme old-xen
  "Created 2019-02-08.")

(custom-theme-set-variables
 'old-xen
 '(mode-line-format (quote ("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position "  " mode-line-modes mode-line-misc-info mode-line-end-spaces))))

(custom-theme-set-faces
 'old-xen
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width semi-condensed :foundry "FBI " :family "Input"))))
 '(avy-lead-face ((t (:background "dark red" :foreground "white"))))
 '(column-enforce-face ((t (:background "#524500"))))
 '(company-scrollbar-bg ((t (:background "#191919"))))
 '(company-scrollbar-fg ((t (:background "#0c0c0c"))))
 '(company-tooltip ((t (:inherit default :background "gray14"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
 '(cov-heavy-face ((t (:foreground "green"))))
 '(cov-light-face ((t (:foreground "orange"))))
 '(cov-none-face ((t (:foreground "red"))))
 '(cursor ((t (:background "turquoise"))))
 '(flycheck-color-mode-line-error-face ((t (:background "firebrick3"))))
 '(flycheck-color-mode-line-info-face ((t (:inherit flycheck-error-list-highlight))))
 '(flycheck-color-mode-line-running-face ((t (:inherit nil :background "light slate blue"))))
 '(flycheck-color-mode-line-warning-face ((t (:inherit flycheck-warning :foreground "#efefef" :weight normal))))
 '(flycheck-error ((t (:background "Red4" :underline (:color "Red1" :style wave)))))
 '(flycheck-fringe-warning ((t (:inherit warning))))
 '(flycheck-warning ((t (:background "DarkOrange4" :underline (:color "DarkOrange" :style wave)))))
 '(flymake-errline ((t (:background "#4b0000"))))
 '(flymake-warnline ((t (:background "#4b1500"))))
 '(hl-line ((t (:background "#222"))))
 '(lsp-ui-sideline-global ((t (:background "dim gray"))))
 '(mode-line ((t (:background "OliveDrab3" :foreground "black"))))
 '(mode-line-inactive ((t (:inherit mode-line :background "grey30" :foreground "grey80" :weight light))))
 '(region ((t (:background "#456"))))
 '(secondary-selection ((((class color) (min-colors 88) (background dark)) (:background "Blue3"))))
 '(vertical-border ((t (:foreground "gray30")) (((type tty)) (:inherit mode-line-inactive)))))

(provide-theme 'old-xen)
