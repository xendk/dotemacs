(server-start)
(setq load-path (cons "~/lib/lisp/" load-path))
(add-to-list 'load-path "~/.emacs.d/drupal-mode/")
(add-to-list 'load-path "~/.emacs.d/php-extras/")
; from http://adamspiers.org/computing/elisp/smooth-scrolling.el via
; http://www.emacswiki.org/emacs/SmoothScrolling
;(load "/home/xen/lib/lisp/smooth-scrolling.el")
(require 'drupal-mode)

(define-key global-map [delete] 'delete-char)
(define-key global-map [M-delete] 'kill-word)
(global-font-lock-mode t)

(define-key global-map [S-menu] 'ido-switch-buffer)
(define-key global-map [C-menu] 'find-file)
(define-key global-map [M-z] 'zap-up-to-char)

(eval-after-load 'php-mode '(require 'php-extras))

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

; http://www.emacswiki.org/emacs/CopyAndPaste
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)


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

 ;; (add-hook 'post-command-hook
 ;;   (lambda ()
 ;;     (recenter '("don't redraw"))))

(iswitchb-mode)

;; log4j for logfile viewing
;; (autoload 'log4j-mode "log4j-mode" "Major mode for viewing log files." t)
;; (add-to-list 'auto-mode-alist '("\\.log\\'" . log4j-mode))


; speedbar fix
(setq dframe-xemacsp nil)

;(speedbar)
;(speedbar-add-supported-extension ".module")
;(speedbar-add-supported-extension ".install")

;(set-language-environment "Latin-1")
;(standard-display-european t)
(set-input-mode (car (current-input-mode))
        (nth 1 (current-input-mode))
        0)

;(require 'ljupdate)
;(setq lj-default-profile (lj-defprofile 'livejournal "blueowl"))

;; Org mode
(setq org-log-done 'time)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
;; (global-set-key [(XF86Back)] 'previous-buffer)
;; (global-set-key [(XF86Forward)] 'next-buffer)

; Setting 'globally':
; (setq-default show-trailing-whitespace t)


;; *** DRUPAL ***


(setq-default indent-tabs-mode nil)

;(add-to-list 'vc-handled-backends 'SVN)

(defvar backward-delete-char-pairwise-alist '((?" _ ?")
					      (?' _ ?')
))
(require 'skeleton)
;(setq skeleton-pair-alist '(
;(?{ ?\n > _ ?\n ?} >)
;))
;(setq skeleton-pair-alist nil)

(defun delete-whitespace ()
  "Delete characters from point up to next non-whitespace char"
  (interactive)
  (let ((here (point)))
    (skip-syntax-forward "-")
    (if (/= (point) here)
	(delete-region (point) here))))

(defun backward-delete-char-pairwise (arg &optional killp)
  "Test.."
  (interactive "*p\nP")
  (let*  ((char (preceding-char))
	  (end-point (save-excursion
		   (progn
		     (skip-syntax-forward "->")
		     (point))))
	  (mchar (save-excursion
		   (progn (goto-char end-point)
		     (message (string (following-char)))
		     (following-char))))
	(skeleton (or ;(assq char '((?{ _ ?})))
		      (assq char skeleton-pair-alist)
		      (assq char skeleton-pair-default-alist)
		      (assq char backward-delete-char-pairwise-alist)
;		      `(,char _ ,char)
		      ))
;	(tmp (message (car (last skeleton))))
)
    (if (and (< 2 (length skeleton)) (= mchar (car (last skeleton))))
	(progn
;(message "here")
	  (delete-region (point) end-point)
(delete-char 1)
(call-interactively 'backward-delete-char-untabify killp))
    ;  (message skeleton)
    (call-interactively 'backward-delete-char-untabify killp)
    )
  )
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

          (push-mark newmark nil t)
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


;; (defun backward-delete-char-pairwise (arg &optional killp)
;;   "Test.."
;;   (interactive "*p\nP")
;;   (let*  ((char (preceding-char)) (mchar (following-char))
;; 	(skeleton (or (assq char skeleton-pair-alist)
;; 		      (assq char skeleton-pair-default-alist)
;; 		      (assq char backward-delete-char-pairwise-alist)
;; ;		      `(,char _ ,char)
;; 		      )))
;;     (if (and (= 3 (length skeleton)) (= mchar (car (cdr (cdr skeleton)))))
;; 	(progn (delete-char 1) (call-interactively 'backward-delete-char-untabify killp))
;;     ;  (message skeleton)
;;     (call-interactively 'backward-delete-char-untabify killp)
;;     )
;;   )
;; )

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
  "Indent if on whitespace, expand tempo or dabbrev-expand."
  (interactive "*")
  (if (or (bolp) ; Beginning of line
          (region-active-p) ; We have an active region
          (eq (char-syntax (char-before)) ?\ ) ; Or whitespace
          )
      (indent-for-tab-command)
    (or
     (tempo-expand-if-complete)
     (call-interactively 'dabbrev-expand)
     )
    )
  )

(require 'tempo)
(setq tempo-interactive t)

(defadvice tempo-define-template (after no-self-insert-in-abbrevs activate)
  "Skip self-insert if template function is called by an abbrev."
  (put (intern (concat "tempo-template-" (ad-get-arg 0))) 'no-self-insert t))

(defun drupal-module-name ()
  "Return name of module"
  (car (split-string (buffer-name) "\\.")))

(tempo-define-template "php-if"
		       '("if (" p ") {" > n>
			 r> n>
			 "}")
		       "if"
		       "Insert an PHP if statement.")


(tempo-define-template "php-else"
		       '("else {" > n>
			 r> n>
			 "}")
		       "el"
		       "Insert an PHP else statement.")

(tempo-define-template "php-elseif"
		       '("elseif (" p ") {" > n>
			 r> n>
			 "}")
		       "ei"
		       "Insert an PHP elseif statement.")

(tempo-define-template "php-function"
		       '("function " p "(" r ") {" > n>
			 r> n
			 "}")
		       "fu"
		       "Insert an PHP function statement.")

(tempo-define-template "php-while"
		       '("while (" p ") {" > n>
			 r> n>
			 "}")
		       "wh"
		       "Insert an PHP while statement.")

(tempo-define-template "php-foreach"
		       '("foreach (" p " as " p ") {" > n>
			 r> n>
			 "}")
		       "fe"
		       "Insert an PHP foreach statement.")

(tempo-define-template "php-for"
		       '("for (" p "; " p "; " p ") {" > n>
			 r> n>
			 "}")
		       "fo"
		       "Insert an PHP foreach statement.")

(tempo-define-template "php-switch"
		       '("switch (" p ") {" > n>
			 r> n>
			 "}")
		       "sw"
		       "Insert an PHP switch statement.")

(tempo-define-template "php-case"
		       '("case " p ":" > n>
			 r> n>
			 "break;")
		       "ca"
		       "Insert an PHP case statement.")

(tempo-define-template "php-default"
		       '("default:" > n>
			 r> n>
			 "break;")
		       "de"
		       "Insert an PHP default statement.")

(tempo-define-template "php-class"
		       '("class " p " {" > n>
			 r> n
			 "}")
		       "cl"
		       "Insert an PHP function class.")

(tempo-define-template "drupal-watchdog"
		       '("watchdog('" p "', '" r "', NULL, WATCHDOG_DEBUG);")
		       "wd"
		       "Insert a Drupal watchdog statement.")

(tempo-define-template "drupal-hook"
		       '("/**" n " * Implementation of hook_"
                         (p "Hook: " hook) "()." n " */" n
                         "function " (drupal-module-name) "_"
                         (s hook) "(" p ") {" > n>
			 r> n
			 "}")
		       "dh"
		       "Insert a Drupal hook function.")

(tempo-define-template "drupal-function"
		       '("/**" n " * " r n " */" n
                         "function " (drupal-module-name) "_"
                         p "(" r ") {" > n>
			 r> n
			 "}")
		       "df"
		       "Insert a Drupal function.")

(tempo-define-template "php-tag"
		       '("<?php " r " ?>")
		       "<?"
		       "Insert a PHP tag.")

;; (tempo-define-template "php-true"
;; 		       '("TRUE")
;; 		       "php-true"
;; 		       "Insert an PHP TRUE.")


;; (define-abbrev php-mode-abbrev-table "ifst"
;;   "" 'tempo-template-php-if)
;; (define-abbrev php-mode-abbrev-table "whst"
;;   "" 'tempo-template-php-while)
;; (define-abbrev php-mode-abbrev-table "fest"
;;   "" 'tempo-template-php-foreach)
;; (define-abbrev php-mode-abbrev-table "function"
;;   "" 'tempo-template-php-function)
;; (define-abbrev php-mode-abbrev-table "true"
;;   "" 'tempo-template-php-true)
;; (define-abbrev php-mode-abbrev-table "<?"
;;   "" 'tempo-template-php-tag)
;; (define-abbrev html-mode-abbrev-table "<?"
;;   "" 'tempo-template-php-tag)

;; (defadvice tempo-define-template (after no-self-insert-in-abbrevs activate)
;;   "Skip self-insert if template function is called by an abbrev."
;;   (put (intern (concat "tempo-template-" (ad-get-arg 0))) 'no-self-insert t))

; This face hackery is stolen from flyspell.
(defvar xen-prog-text-faces
  '(font-lock-string-face font-lock-comment-face font-lock-doc-face)
  "Faces corresponding to text in programming-mode buffers.")

(defun xen-generic-progmode-verify ()
  "Check if we're in a comment or string."
  (let ((f (get-text-property (point) 'face)))
    (memq f xen-prog-text-faces)))

; (not (funcall xen-generic-check-word-p))


(defun xen-skeleton-pair-insert-maybe ()
  "Inserts pairs when not in strings and comments."
  (interactive "*")
  (if (not (xen-generic-progmode-verify))
      (call-interactively 'skeleton-pair-insert-maybe)
    (call-interactively 'self-insert-command)
    )
  )

(require 'misc)

(defun my-php-mode-hook ()
  (setq skeleton-pair t)
  (local-set-key (kbd "[") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "{") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "<") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "'") 'xen-skeleton-pair-insert-maybe)
  (local-set-key (kbd "C-$") 'hs-toggle-hiding)
  (local-set-key [backspace] 'xen-paired-delete-backward)
  (local-set-key [delete] 'xen-paired-delete)
  (local-set-key [tab] 'xen-tab)
  (local-set-key [S-iso-lefttab] 'indent-for-tab-command)
  (local-set-key [C-tab] 'dabbrev-expand)
  (local-set-key [M-up] 'tempo-backward-mark)
  (local-set-key [M-down] 'tempo-forward-mark)
  (local-set-key (kbd "«") 'tempo-backward-mark)
  (local-set-key (kbd "»") 'tempo-forward-mark)
  (local-set-key (kbd "M-Z") 'zap-up-to-char)
  ;; (local-set-key (kbd "M-z") 'zap-up-to-char)
  (flyspell-prog-mode)
  (local-set-key [S-return] 'php-end-new-line)
;  (local-set-key [?\S- ] 'php-end-line)
  (local-set-key (kbd "M-,") 'php-end-line)
  (modify-syntax-entry ?_ "_" php-mode-syntax-table)
  )
(add-hook 'php-mode-hook 'my-php-mode-hook)

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

; doesn't work right
(defun other-kill-buffer (&optional window)
  "Kills other buffer"
  (interactive "P")
  (let oldwin (get-buffer-window (current-buffer))
    (save-excursion
      (other-window
       (if window window 1))
      (kill-buffer nil)
      (set-window oldwin)
      ))
)

;(define-key global-map [?\C-c ?\C-k] 'kill-buffer-delete-window)
(define-key global-map [?\C-c ?\C-k] 'other-kill-buffer)
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

;; *** IDO ***

(setq ido-enable-flex-matching t)

(setq ido-execute-command-cache nil)

(defun ido-execute-command ()
  (interactive)
  (call-interactively
   (intern
    (ido-completing-read
     "M-x "
     (progn
       (unless ido-execute-command-cache
         (mapatoms (lambda (s)
                     (when (commandp s)
                       (setq ido-execute-command-cache
                             (cons (format "%S" s) ido-execute-command-cache))))))
       ido-execute-command-cache)))))

(add-hook 'ido-setup-hook
          (lambda ()
            (setq ido-enable-flex-matching t)
            (global-set-key "\M-x" 'ido-execute-command)))


; Check: http://www.xsteve.at/prg/emacs/power-user-tips.html
; (require 'ffap)
; (ffap-bindings)

(setq next-line-add-newlines nil)
(setq hs-minor-mode-hook nil)
;(hscroll-global-mode)

; Insert default contents into new files if variable `auto-insert' is non-nil.
; Matches the visited file name against the elements of `auto-insert-alist'.
(add-hook 'find-file-hooks 'auto-insert)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ange-ftp-try-passive-mode t)
 '(auto-insert-mode t nil (autoinsert))
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program "google-chrome")
 '(comint-input-autoexpand t)
 '(css-indent-offset 2)
 '(delete-selection-mode t nil (delsel))
 '(desktop-files-not-to-save "^/none[^/:]*:")
 '(desktop-restore-eager 3)
 '(desktop-save (quote ask-if-new))
 '(desktop-save-mode nil)
 '(drupal/emacs-drush-update-tags-after-save t)
 '(ediff-keep-variants nil)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-use-last-dir t)
 '(flymake-cursor-error-display-delay 0.3)
 '(flymake-cursor-number-of-errors-to-display 10)
 '(flyspell-default-dictionary "british")
 '(hscroll-margin 15)
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(js-indent-level 2)
 '(mouse-avoidance-mode (quote animate) nil (avoid))
 '(mouse-sel-default-bindings t)
 '(mouse-sel-mode t nil (mouse-sel))
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 5) ((control)))))
 '(mouse-yank-at-point t)
 '(mutt-file-pattern "mutt-[a-z]+-[0-9]+-[0-9]+-[0-9]+")
 '(org-agenda-files (quote ("~/Documents/Todo.org")))
 '(org-support-shift-select t)
 '(php-file-patterns (quote ("\\.php[s34]?\\'" "\\.phtml\\'" "\\.inc\\'" "\\.module\\'")))
 '(query-replace-highlight t)
 '(save-place t nil (saveplace))
 '(server-temp-file-regexp "^\\(/tmp/Re\\|/draft\\)\\|/tmp/\\(rat\\.\\|mutt-\\).*$")
 '(show-paren-mode t)
 '(speedbar-fetch-etags-command "etags" t)
 '(speedbar-hide-button-brackets-flag nil)
 '(speedbar-ignored-modes nil t)
 '(speedbar-show-unknown-files t)
 '(speedbar-supported-extension-expressions (quote ("\\.\\(inc\\|php[s34]?\\)" ".[ch]\\(\\+\\+\\|pp\\|c\\|h\\|xx\\)?" ".tex\\(i\\(nfo\\)?\\)?" ".el" ".emacs" ".l" ".lsp" ".p" ".java" ".f\\(90\\|77\\|or\\)?" ".ada" ".pl" ".tcl" ".m" ".scm" ".pm" ".py" ".s?html" "Makefile\\(\\.in\\)?" ".p\\(hp\\(3\\|4\\)\\|html\\|hp\\)" ".module" ".install")))
 '(speedbar-track-mouse-flag t)
 '(speedbar-update-flag (quote x) t)
 '(tex-command nil t)
 '(tex-dvi-view-command "dvilx")
 '(tool-bar-mode nil nil (tool-bar))
 '(uniquify-after-kill-buffer-p t)
 '(uniquify-ask-about-buffer-names-p t)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(uniquify-trailing-separator-p t)
 '(url-cookie-confirmation (quote nil))
 '(url-honor-refresh-requests (quote t))
 '(url-privacy-level (quote (email)))
 '(w3-default-homepage (quote "http://hades/~beast"))
 '(w3-default-stylesheet (quote nil))
 '(w3-delay-image-loads (quote t))
 '(w3-honor-stylesheets (quote t))
 '(w3-image-mappings (quote (("image/x-xbitmap" . xbm) ("image/xbitmap" . xbm) ("image/xbm" . xbm) ("image/jpeg" . jpeg) ("image/gif" . gif) ("image/png" . png) ("image/x-fax" . g3fax) ("image/x-raster" . rast) ("image/windowdump" . xwd) ("image/x-icon" . icon) ("image/portable-graymap" . pgm) ("image/portable-pixmap" . ppm) ("image/x-pixmap" . xpm) ("image/x-xpixmap" . xpm) ("image/pict" . pict) ("image/x-rgb" . sgi) ("image/x-sgi" . sgi) ("image/x-macpaint" . macpt) ("image/x-targa" . tga) ("image/tiff" . tiff))))
 '(w3-load-hook (quote nil))
 '(w3-mode-hook (quote nil))
 '(w3-netscape-compatible-comments (quote t))
 '(w3-preferences-cancel-hook (quote nil) t)
 '(w3-preferences-default-hook (quote nil) t)
 '(w3-preferences-ok-hook (quote nil) t)
 '(w3-preferences-setup-hook (quote nil) t)
 '(w3-source-file-hook (quote nil))
 '(w3-toolbar-orientation (quote default) t)
 '(w3-toolbar-type (quote both) t)
 '(w3-use-menus (quote (file edit view go bookmark options buffers style emacs nil help)))
 '(w3-user-colors-take-precedence (quote nil)))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(region ((t (:background "#456"))))
 '(secondary-selection ((((class color) (min-colors 88) (background dark)) (:background "Blue3")))))
(put 'narrow-to-region 'disabled nil)
