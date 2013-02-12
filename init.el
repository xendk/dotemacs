;; Xens emacs configuration.
(set-face-attribute 'default nil :font "Anonymous Pro-11")
; Fun-mode:
; (set-face-attribute 'default nil :font "Monofur-12")

;; Handy trick:
;; (set-face-attribute 'default nil :height 140)

;; Handy variables for line (non-)wrapping:
;; trucante-lines
;; word-wrap
;; Also, look into adaptive-wrap (mentioned here: http://emacswiki.org/emacs/LineWrap )

(server-start)

;; Beginning of the el4r block:
;; RCtool generated this block automatically. DO NOT MODIFY this block!
;(add-to-list 'load-path "/usr/share/emacs/site-lisp")
;(require 'el4r)
;(el4r-boot)
;; End of the el4r block.
;; User-setting area is below this line.

;; Lets get rid of the menu bar.
(menu-bar-mode -1)
(setq load-path (cons "~/lib/lisp/" load-path))
(setq load-path (cons "~/.emacs.d/lib/" load-path))

;; Setup Android SDK.
(setq android-mode-sdk-dir "~/lib/android-sdk-linux/")
;; And load it's .el file.
(load (concat android-mode-sdk-dir "tools/lib/android.el"))

(require 'package)
(add-to-list 'package-archives
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

; Try out http://www.emacswiki.org/emacs/MiniMap ?

; Solarized color scheme.
;(add-to-list 'load-path "~/.emacs.d/emacs-color-theme-solarized/")
;(require 'color-theme-solarized)

(add-to-list 'load-path "~/.emacs.d/emacs-powerline/")
(require 'powerline)

(add-to-list 'load-path "~/.emacs.d/drupal-mode/")
; from http://adamspiers.org/computing/elisp/smooth-scrolling.el via
; http://www.emacswiki.org/emacs/SmoothScrolling
;(load "/home/xen/lib/lisp/smooth-scrolling.el")
(require 'drupal-mode)

(define-key global-map [delete] 'delete-char)
(define-key global-map [M-delete] 'kill-word)
(global-font-lock-mode t)

(define-key global-map (kbd "M-Z") 'zap-up-to-char)
(defun xen-emacs-init ()
  "Open ~/.emacs.d/init.el."
  (interactive)
  (find-file "~/.emacs.d/init.el")
)
(define-key global-map (kbd "C-c e") 'xen-emacs-init)

(setq gtags-suggested-key-mapping nil) ; Keymap? Yes, please
(require 'gtags)
(add-hook 'gtags-mode-hook
          #'(lambda()
              (define-key gtags-mode-map [(meta \,)] 'gtags-find-rtag)
              (define-key gtags-mode-map [(meta .)] 'gtags-find-tag)
              (define-key gtags-mode-map [(control t)] nil)
              ;; (define-key gtags-mode-map 'button2 nil)
              (define-key gtags-mode-map [mouse-2] nil)
              (define-key gtags-mode-map [mouse-3] nil)
              ))

; Activate flyspell in magit commit buffer.
(defun xen-magit-log-edit-mode-hook ()
  (flyspell-mode)
)
(add-hook 'magit-log-edit-mode-hook 'xen-magit-log-edit-mode-hook)
; Add shortcut to open magit status buffer.
(global-set-key (kbd "C-c C-g") 'magit-status)

;; TODO: Anything: http://www.emacswiki.org/emacs/Anything or rather http://emacs-helm.github.com/helm/

(helm-mode 1)
(require 'helm-files)
(define-key global-map (kbd "C-x C-f") 'helm-for-files)


;; TODO: https://github.com/rolandwalker/fixmee

;; TODO: CamelCase <-> snake_case conversion:
;; https://gist.github.com/846766
;; https://github.com/emacsmirror/s

; Start EmacsRocks
; Really cool stuff: https://github.com/magnars
; From these:
; https://www.youtube.com/watch?v=p3Te_a-AGqM&feature=player_embedded#!
; http://emacsrocks.com/

(add-to-list 'load-path "~/.emacs.d/mark-multiple/")
(require 'inline-string-rectangle)
(global-set-key (kbd "C-x r t") 'inline-string-rectangle)

(require 'mark-more-like-this)
(global-set-key (kbd "C-<") 'mark-previous-like-this)
(global-set-key (kbd "C->") 'mark-next-like-this)
(global-set-key (kbd "C-M-m") 'mark-more-like-this) ; like the other two, but takes an argument (negative is previous)
(global-set-key (kbd "C-*") 'mark-all-like-this)
(global-set-key (kbd "C-%") 'mark-all-like-this-in-region)

;; Experimental multiple-cursors
(add-to-list 'load-path "~/.emacs.d/multiple-cursors/")
(require 'multiple-cursors)
(set-face-background 'mc/cursor-face "Turquoise")  ;; Emacs 22 Only

(add-to-list 'load-path "~/.emacs.d/expand-region/")
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-S-SPC") 'er/expand-region)

(defun xen-php-mark-next-accessor ()
  "Presumes that current symbol is already marked, skips over one
arrow and marks next symbol."
  (interactive)
  (when (use-region-p)
    (when (< (point) (mark))
      (exchange-point-and-mark))
    (let ((symbol-regexp "\\s_\\|\\sw"))
      (when (looking-at "->")
        (forward-char 2)
        (skip-syntax-forward "_w")
        (exchange-point-and-mark)))))

(defun xen-php-mark-method-call-or-array ()
  "Mark the current symbol (including arrow) and then paren/brace to closing paren/brace."
  (interactive)
  (let ((symbol-regexp "\\s_\\|\\sw\\|->\\|>"))
    (when (or (looking-at symbol-regexp)
              (looking-back symbol-regexp))
      (skip-syntax-backward "_w.")
      (set-mark (point))
      (while (looking-at symbol-regexp)
        (forward-char))
      (if (looking-at "(\\|\\[")
          (forward-list))
      (exchange-point-and-mark))))

(defun php-mode-expantions ()
      (make-variable-buffer-local 'er/try-expand-list)
      (setq er/try-expand-list '(er/mark-word er/mark-symbol er/mark-symbol-with-prefix xen-php-mark-next-accessor xen-php-mark-method-call-or-array er/mark-comment er/mark-comment-block er/mark-inside-quotes er/mark-outside-quotes er/mark-inside-pairs er/mark-outside-pairs)))

(add-hook 'php-mode-hook 'php-mode-expantions)

; Writable grep buffer.
(add-to-list 'load-path "~/.emacs.d/wgrep/")
(require 'wgrep)

(add-to-list 'load-path
              "~/.emacs.d/yasnippet")
(require 'yasnippet)
;; Initialize yasnippet. It's enabled per mode.
;; todo: seems to have some issues with undo and xen-tab.
(yas/reload-all)

;; this would be nice:
;; # -*- mode: snippet -*-
;; # name: Drupal hook
;; # key: dh
;; # --
;; /**
;;  * Implements hook_$1().
;;  */
;; function `(drupal-module-name)`_${1:name}(${2:${1:$(unless yas/modified-p (drupal-get-function-args yas/text (drupal-major-version)))}}) {
;;   $0
;; }

;; http://www.emacswiki.org/emacs/iy-go-to-char.el or
;; https://github.com/lewang/jump-char/blob/master/jump-char.el ?

; End EmacsRocks

; Flymake-phpcs
; https://github.com/illusori/emacs-flymake-phpcs
; alternative: https://github.com/mcfunley/dotemacs/blob/master/php-flymake.el
; addon: https://github.com/illusori/emacs-flymake
; addon: https://github.com/illusori/emacs-flymake-cursor
;; (setq flymake-phpcs-command "~/lib/emacs-flymake-phpcs/bin/flymake_phpcs")
;; (setq flymake-phpcs-standard
;;   "DrupalCodingStandard")
;; (setq flymake-phpcs-standard
;;  "/home/xen/lib/drupalcs/DrupalCodingStandard/ruleset.xml")
;; Show the name of sniffs in warnings (eg show
;; "Generic.CodeAnalysis.VariableAnalysis.UnusedVariable" in an unused
;; variable warning)
;; (setq flymake-phpcs-show-rule nil)

(add-to-list 'load-path "~/lib/emacs-flymake-cursor/")
(eval-after-load 'flymake '(require 'flymake-cursor))
(load "/home/xen/lib/emacs-flymake-phpcs/flymake-phpcs.el")
(require 'flymake-phpcs)

; Fixes flymake-phpcs, when the file has been accessed through a
; symlink in its path.
(defun flymake-create-temp-inplace (file-name prefix)
  (unless (stringp file-name)
    (error "Invalid file-name"))
  (or prefix
      (setq prefix "flymake"))
  (let* ((temp-name   (file-truename (concat (file-name-sans-extension file-name)
			      "_" prefix
			      (and (file-name-extension file-name)
				   (concat "." (file-name-extension file-name)))))))
    (flymake-log 3 "create-temp-inplace: file=%s temp=%s" file-name temp-name)
    temp-name))

; --
; look into this: https://github.com/openist/drupal-emacs/blob/master/.emacs
; (add-hook 'window-setup-hook 'resume-windows)


; http://www.emacswiki.org/emacs/WinnerMode
(when (fboundp 'winner-mode)
  (winner-mode 1))
(global-set-key [(XF86Back)] 'winner-undo)
(global-set-key [(XF86Forward)] 'winner-redo)


; http://www.emacswiki.org/emacs/WindowResize
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

; http://www.emacswiki.org/emacs/WindMove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
; http://www.emacswiki.org/emacs/FrameMove
(require 'framemove)
(setq framemove-hook-into-windmove t)

; http://www.emacswiki.org/emacs/WholeLineOrRegion
(require 'whole-line-or-region)

; http://emacswiki.org/emacs/CopyingWholeLines
;; duplicate current line
(defun duplicate-current-line (&optional n)
  "duplicate current line, make more than 1 copy given a numeric argument"
  (interactive "p")
  (save-excursion
    (let ((nb (or n 1))
    	  (current-line (thing-at-point 'line)))
      ;; when on last line, insert a newline first
      (when (or (= 1 (forward-line 1)) (eq (point) (point-max)))
    	(insert "\n"))

      ;; now insert as many time as requested
      (while (> n 0)
    	(insert current-line)
    	(decf n)))))

(global-set-key (kbd "C-S-d") 'duplicate-current-line)

; http://www.emacswiki.org/emacs/CopyAndPaste
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
; As I never use C-v anyway, and its effect when i hit it confuses me,
; why not bind it to pasting from outside (like the middle button
; does)?
(global-set-key "\C-v" 'xen-paste)
(defun xen-paste ()
  "Paste from outside."
  (interactive)
(insert (x-selection-value 'CLIPBOARD)))

; Don't switch to a frame already containing the selected buffer, but
; show the same buffer in a new frame.
(setq ido-default-buffer-method 'selected-window)

; http://www.emacswiki.org/emacs/MoveRegion
(defun move-region (start end n)
  "Move the current region up or down by N lines."
  (interactive "r\np")
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (let ((start (point)))
      (insert line-text)
      (setq deactivate-mark nil)
      (set-mark start))))

(defun move-region-up (start end n)
  "Move the current line up by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) -1 (- n))))

(defun move-region-down (start end n)
  "Move the current line down by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) 1 n)))

;; (global-set-key (kbd "M-<up>") 'move-region-up)
;; (global-set-key (kbd "M-<down>") 'move-region-down)
(global-set-key (kbd "S-M-<up>") 'move-region-up)
(global-set-key (kbd "S-M-<down>") 'move-region-down)

; http://www.emacswiki.org/emacs/SwapRegions
(defun swap-regions (beg1 end1 beg2 end2)
  "Swap region between BEG1 and END1 with region BEG2 and END2.

For the first region, mark the first region and set mark at
point.  The second region only needs to be marked normally.
Again, set the mark at the beginning and end of the first region,
then mark the second region with mark and point.

The order of the two regions in the buffer doesn't matter.
Either one can precede the other.  However, the regions can not
be swapped if they overlap.

All arguments can either be a number for a position in the buffer
or a marker."
  (interactive
   (if (< (length mark-ring) 2)
       (error "Not enough in mark-ring to swap a region")
     (let ((region (list (region-beginning) (region-end)))
	   (marks (sort (list (marker-position (car mark-ring))
			      (marker-position (cadr mark-ring)))
			'<)))
       (if (< (car region) (car marks))
	   (append region marks)
	 (append marks region)))))
  (if (or (and (< beg2 beg1) (< beg1 end2))
	  (and (< beg1 beg2) (< beg2 end1)))
      (error "Unable to swap overlapping regions")
      (save-excursion
	(insert
	 (prog1 (delete-and-extract-region beg2 end2)
	   (goto-char beg2)
	   (insert
	    (delete-and-extract-region beg1 end1))
	   (goto-char beg1))))))


; Todo: http://www.emacswiki.org/cgi-bin/wiki.pl?ColorTheme

;; In every buffer, the line which contains the cursor will be fully
;; highlighted
(global-hl-line-mode 1)
(set-face-background 'hl-line "#222")  ;; Emacs 22 Only

; Let's set the frame colors too.
(setq default-frame-alist '((background-color . "black")
                            (foreground-color . "white")))

(iswitchb-mode)

; speedbar fix
(setq dframe-xemacsp nil)

(set-input-mode (car (current-input-mode))
        (nth 1 (current-input-mode))
        0)


(setq org-log-done 'time)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

; Setting 'globally':
; (setq-default show-trailing-whitespace t)


;; *** DRUPAL ***

(setq-default indent-tabs-mode nil)

(defun xen-open ()
"Open new line, with proper indentation."
(interactive)
(call-interactively 'move-beginning-of-line)
(call-interactively 'open-line)
(indent-for-tab-command)
)

(defun xen-paired-delete (arg &optional killp)
  "Test.."
  (interactive "*p\nP")
  (let*
      (
       (here (point))
       (newmark (xen-find-matching here))
       )
    (if (and newmark (< 0 newmark))
        (progn
          (save-excursion
            (if (< here newmark)
                (progn
                  (goto-char newmark)
                  (delete-backward-char 1)
                  (goto-char here)
                  (call-interactively 'delete-char)
                  (setq newmark (- newmark 2))
                  )
              (progn
                (goto-char newmark)
                (delete-char 1)
                (goto-char (- here 1))
                (call-interactively 'delete-char)
                )
              )
            )

          (push-mark newmark nil t) ; todo: doesn't work? or does it?
          )
      (call-interactively 'delete-char (list arg killp))
      )
    )
  )

(defun xen-paired-delete-backward (arg &optional killp)
  "Test.."
  (interactive "*p\nP")
  (let*
      (
       (here (point))
       (newmark (xen-find-matching (- here 1)))
       )
    (if (and newmark (< 0 newmark))
        (progn
          (save-excursion
            (if (< here newmark)
                (progn
                  (goto-char newmark)
                  (delete-backward-char 1)
                  (goto-char here)
                  (call-interactively 'delete-backward-char killp)
                  (setq newmark (- newmark 2))
                  )
              (progn
                (goto-char newmark)
                (delete-char 1)
                (goto-char (- here 1))
                (call-interactively 'delete-backward-char killp)
                )
              )
            )

          (push-mark newmark nil t)
          )
      (call-interactively 'backward-delete-char-untabify killp)
      )
    )
  )
; http://www.emacswiki.org/cgi-bin/wiki/delsel.el
(put 'xen-paired-delete-backward 'delete-selection 'supersede)
(put 'xen-paired-delete 'delete-selection 'supersede)

(defvar xen-delete-char-pairwise-alist '(
                                         (?" ?" 0)
                                         (?' ?' 0)
                                         (?{ ?} 1)
                                         (?} ?{ -1)
                                         (?( ?) 1)
                                         (?) ?( -1)
                                         (?[ ?] 1)
                                         (?] ?[ -1)
))

(defun xen-find-matching (pos)
  (let
      (newmark)
    (progn
      (save-excursion
        (progn
          (goto-char pos)
          (let*
              ((char (following-char))
               (pairing (assq char xen-delete-char-pairwise-alist))
               (deactivate-mark)
               )
            (if pairing
                (let
                    ((apair (nth 0 pairing))
                     (bpair (nth 1 pairing))
                     (direction (nth 2 pairing)))
                  (if (= direction 1)
                      (progn ; forward
                                        ; TODO scan-lists chokes on mismached..
                        (setq newmark (scan-lists pos 1 0))
(message (string newmark))
                        (if (= (char-before newmark) bpair) () (setq newmark nil))
                        )

                    (if (= direction -1)
                        (progn ; backwards
                          (setq newmark (scan-lists pos -1 1))
                          (if (= (char-after newmark) bpair) () (setq newmark nil))
                          )
                      (progn ; figure it out
                        (let (
                              (f (get-text-property (- pos 1) 'face))
                              (f2 (get-text-property (+ pos 1) 'face))
                              )
                          (progn
                                        ; TODO check the other direction and cop out if it's comment/string too. Think it's done
                                        ; TODO doesn't deal well with backspace in the middle of ''. Should be fixed by killing forward-char below.

                            (setq direction (if (memq f xen-prog-text-faces)
                                                 (progn
                                                   (if (memq f2 xen-prog-text-faces) 0 -1) ; Check the other direction and cop out if it too is a comment
                                                   )
                                              1
                                              )
                                  )
                            ;(message (string direction))
                            )
                          )
                        (setq newmark
                              (if (= direction 1)
                                  (progn
                                    ;(forward-char)
                                    (re-search-forward (concat "[^\\]" (list bpair))))
                                (if (= direction -1)
                                    (progn
                                      (+ (re-search-backward (concat "[^\\]" (list bpair))) 1)
                                      )
                                  (progn ; 0 case, cop out
                                   (setq newmark nil)
                                   )
                                  )
                                )
                              )
                        )
                      )
                    )
                  )
              )
            )
          )
        newmark
        )
      )
    )
  )

(defun php-end-new-line ()
"Test"
(interactive)
(move-end-of-line 1)
(if (= ?\; (preceding-char))
()
(insert ";"))
(newline-and-indent)
)

(defun php-end-line ()
  "Test"
  (interactive)
  (save-excursion
    (progn
      (move-end-of-line 1)
      (if (= ?\; (preceding-char))
	  ()
	(insert ";"))
      ))
  )

(defun test-func ()
(interactive)
(message (char-to-string (char-syntax (char-after)))))

(defun xen-tab ()
  "Indent if on whitespace or dabbrev-expand."
  (interactive "*")
  (if (or (bolp) ; Beginning of line
          (region-active-p) ; We have an active region
          (eq (char-syntax (char-before)) ?\ ) ; Or whitespace
          )
      (indent-for-tab-command)
    (or
     ;; (yas/expand)
     (call-interactively 'dabbrev-expand)
     )
    )
  )

; This face hackery is stolen from flyspell.
(defvar xen-prog-text-faces
  '(font-lock-string-face font-lock-comment-face font-lock-doc-face)
  "Faces corresponding to text in programming-mode buffers.")

;; https://github.com/capitaomorte/autopair
(add-to-list 'load-path "~/.emacs.d/autopair/")
(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers
(setq autopair-skip-whitespace t)
(setq autopair-blink-delay 0.05) ;; Even shorter delay.

(require 'misc)

(defun xen-coding-common-bindings ()
  (local-set-key (kbd "C-o") 'xen-open)
  (local-set-key [backspace] 'xen-paired-delete-backward)
  (local-set-key [delete] 'xen-paired-delete)
  (local-set-key [tab] 'xen-tab)
  (local-set-key [S-iso-lefttab] 'indent-for-tab-command)
  (local-set-key [C-tab] 'dabbrev-expand)
  (flyspell-prog-mode)
)

(defun my-php-mode-hook ()
  (xen-coding-common-bindings)
  (local-set-key [S-return] 'php-end-new-line)
  (local-set-key (kbd "M-,") 'php-end-line)
  ;; Work around bug in Emacs 23.3.1 cc-mode c-fill-paragraph
  ;; http://superuser.com/questions/250442/fixing-c-fill-paragraph-with-comments-in-emacs-23-2-1
  (local-set-key (kbd "M-q") 'fill-paragraph)
  (modify-syntax-entry ?_ "_" php-mode-syntax-table)
  (yas/minor-mode 1)
  )
(add-hook 'php-mode-hook 'my-php-mode-hook)

(add-hook 'js-mode-hook 'xen-js-mode-hook)
(defun xen-js-mode-hook ()
  (xen-coding-common-bindings)
  (yas/minor-mode 1)
  )

(add-hook 'css-mode-hook 'xen-css-mode-hook)
(defun xen-css-mode-hook ()
  (xen-coding-common-bindings)
  (yas/minor-mode 1)
  )

(defun php-doc-paragraph-boundaries ()
  (setq paragraph-separate "^[ \t]*\\(\\(/[/\\*]+\\)\\|\\(\\*+/\\)\\|\\(\\*?\\)\\|\\(\\*?[ \t]*@[[:alpha:]]+\\([ \t]+.*\\)?\\)\\)[ \t]*$")
  (setq paragraph-start (symbol-value 'paragraph-separate)))

(add-hook 'php-mode-user-hook 'php-doc-paragraph-boundaries)

(defun kill-buffer-delete-window () "" (interactive)
  (kill-buffer nil)
  (delete-window)
)

(defun other-kill-buffer-delete-window () "Kills other window and buffer" (interactive)
  (other-window 1)
  ;(kill-buffer-delete-window)
  (kill-buffer-and-window)
)

(define-key global-map [?\C-c ?k] 'other-kill-buffer-delete-window)
(define-key global-map [print] 'speedbar-get-focus)
(define-key global-map [S-print] 'speedbar)

(autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t)
(autoload 'xml-mode "psgml" "Major mode to edit XML files." t)

(setq interpreter-mode-alist (append '(("php" . php-mode))
         interpreter-mode-alist))


(defvar inverse-mode nil)
(defun inverse () "" (interactive)
  (if inverse-mode
      (progn (set-foreground-color "Black")
	     (set-cursor-color "Turquoise")
	     (set-background-color "White")
             (set-face-background 'hl-line "#DDD")
	     (setq inverse-mode nil))
    (progn (set-foreground-color "White")
	   (set-cursor-color "Turquoise")
	   (set-background-color "Black")
           (set-face-background 'hl-line "#222")
	   (setq inverse-mode t))
    )
  )

(set-cursor-color "Turquoise")
(transient-mark-mode t)
(show-paren-mode)
(setq mark-even-if-inactive t)
(setq bookmark-save-flag 1)
(add-hook 'kill-emacs-hook (function (lambda () (server-start 1))))
(setq font-lock-maximum-decoration t)

(global-set-key [?\C-x ?\C-b] 'buffer-menu)
(add-hook 'c-mode-hook
	  '(lambda ()
	     (define-key c-mode-map [f3] 'compile)
	     (define-key c-mode-map [f4] 'gdb)
	     ))
(add-hook 'c++-mode-hook
	  '(lambda ()
	     (define-key c++-mode-map [f3] 'compile)
	     (define-key c++-mode-map [f4] 'gdb)
	     ))

(defun my-c-mode-common-hook ()
  ;; my customizations for all of c-mode, c++-mode, objc-mode, java-mode
  (c-set-offset 'substatement-open 0)
  (define-key c-mode-base-map [return] 'newline-and-indent)
  (define-key c-mode-base-map "\C-j" 'newline)
  (define-key c-mode-base-map [C-return] 'newline)
  (define-key c-mode-base-map [tab] 'dabbrev-expand)
  (define-key c-mode-base-map [S-iso-lefttab] 'indent-for-tab-command)
  (define-key c-mode-base-map [C-tab] 'tab-to-tab-stop)
  )
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(defun my-tcl-mode-hook ()
  (define-key tcl-mode-map [return] 'newline-and-indent)
  (define-key tcl-mode-map "\C-j" 'newline)
  (define-key tcl-mode-map [C-return] 'newline)
  (define-key tcl-mode-map [tab] 'dabbrev-expand)
  (define-key tcl-mode-map [S-iso-lefttab] 'indent-for-tab-command)
  (define-key tcl-mode-map [C-tab] 'tab-to-tab-stop)
  )
(add-hook 'tcl-mode-hook 'my-tcl-mode-hook)
(defun my-perl-mode-hook ()
  (define-key perl-mode-map [return] 'newline-and-indent)
  (define-key perl-mode-map "\C-j" 'newline)
  (define-key perl-mode-map [C-return] 'newline)
  (define-key perl-mode-map [tab] 'dabbrev-expand)
  (define-key perl-mode-map [S-iso-lefttab] 'indent-for-tab-command)
  (define-key perl-mode-map [C-tab] 'tab-to-tab-stop)
  )
(add-hook 'perl-mode-hook 'my-perl-mode-hook)
(defun my-ruby-mode-hook ()
  (define-key ruby-mode-map [return] 'newline-and-indent)
  (define-key ruby-mode-map "\C-j" 'newline)
  (define-key ruby-mode-map [C-return] 'newline)
  (define-key ruby-mode-map [tab] 'dabbrev-expand)
  (define-key ruby-mode-map [S-iso-lefttab] 'indent-for-tab-command)
  (define-key ruby-mode-map [C-tab] 'tab-to-tab-stop)
  )
(add-hook 'ruby-mode-hook 'my-ruby-mode-hook)

;; Properly handle annotations in java-mode.
(require 'java-mode-indent-annotations)
(add-hook 'java-mode-hook 'java-mode-indent-annotations-setup)

;; *** IDO ***

(setq ido-enable-flex-matching t)

; Check: http://www.xsteve.at/prg/emacs/power-user-tips.html
; (require 'ffap)
; (ffap-bindings)

(setq next-line-add-newlines nil)
(setq hs-minor-mode-hook nil)

; Insert default contents into new files if variable `auto-insert' is non-nil.
; Matches the visited file name against the elements of `auto-insert-alist'.
(add-hook 'find-file-hooks 'auto-insert)

;; Relocate and load customs (so we don't clutter init.el with them).
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file)
