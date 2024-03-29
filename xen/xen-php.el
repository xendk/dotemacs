;;; xen-php.el --- Helper functions for PHP.         -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Thomas Fini Hansen

;; Author: Thomas Fini Hansen <xen@xen.dk>
;; Keywords: local

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'dash)
(require 'smartparens)
(require 'xen)

;; External variables referenced.
(defvar flycheck-phpcs-standard)
(defvar flycheck-php-phpcs-executable)
(defvar er/try-expand-list)
(defvar sp-last-wrapped-region)
(defvar symbol-regexp)

;; expand-region stuff.
(defun xen-php-mark-next-accessor ()
  "Presuming that current symbol is already marked, skip over one arrow and mark the next symbol."
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
              (looking-back symbol-regexp 1))
      (skip-syntax-backward "_w.")
      (set-mark (point))
      (while (looking-at symbol-regexp)
        (forward-char))
      (if (looking-at "(\\|\\[")
          (forward-list))
      (exchange-point-and-mark))))

(defun xen-php-mode-expansions ()
  "My expand-region setup for php-mode."
  (make-local-variable 'er/try-expand-list)
  (setq er/try-expand-list '(er/mark-subword
                             er/mark-word
                             er/mark-symbol
                             er/mark-symbol-with-prefix
                             xen-php-mark-next-accessor
                             xen-php-mark-method-call-or-array
                             er/mark-comment
                             er/mark-comment-block
                             er/mark-inside-quotes
                             er/mark-outside-quotes
                             er/mark-inside-pairs
                             er/mark-outside-pairs)))

(defun xen-php-spec ()
  "Set PHPCS to use PHPSpec standard."
  (interactive)
  (make-local-variable 'flycheck-phpcs-standard)
  (setq flycheck-phpcs-standard "/home/xen/.config/composer/vendor/kmcculloch/phpspec-code-sniffer/PHPSpec/"))

(defvar xen-php-bin-dir "vendor/bin"
  "Directory for composer installed tools. Defaults to `vendor/bin'.")

(defun xen-php-setup-tools ()
  "Point flycheck towards composer installed tools."
  (when (buffer-file-name)
    (let ((tools (list '("phpcs" . flycheck-php-phpcs-executable) '("phpstan" . phpstan-executable)))
          (composer-root (locate-dominating-file (buffer-file-name) "composer.json")))
      (while (and tools composer-root)
        (dolist (tool tools)
          (let ((file (concat (concat (file-name-as-directory (concat composer-root xen-php-bin-dir))) (car tool))))
            (when (file-exists-p file)
              (make-variable-buffer-local (cdr tool))
              (set (cdr tool) file)
              (delete tool tools))))
        (setq composer-root (locate-dominating-file
                             (unless (equal "/" composer-root)
                               (file-name-directory (directory-file-name composer-root))) "composer.json"))))))

;; Geben hackery.
;; (defun xen-geben-open ()
;;   "Open the current buffer in geben."
;;   (interactive)
;;   (progn
;;     (let ((geben-current-session (car geben-sessions)))
;;       (geben-open-file (geben-source-fileuri geben-current-session (buffer-file-name))))))

;; These was originally copied from the smartparens' authors personal
;; configuration at
;; https://github.com/Fuco1/.emacs.d/blob/master/files/smartparens.el
;; Some modifications has been made.
(defun xen-php-wrap-handler (_id action _context)
  "Wrap block properly.
Inserts newline in after the start brace and before the end
brace, if needed.

ACTION is the currently run wrapping action, we're only interested in `'wrap'.

Indentation is assumed to be handled by indentinator."
  (when (eq action 'wrap)
    (sp-get sp-last-wrapped-region
      ;; Don't bother if both ends are on the same line.
      (unless (eq (line-number-at-pos :beg-in) (line-number-at-pos :end-in))
        (save-excursion
          (goto-char :beg-in)
          ;; Add newline if there's anything but white-space after the
          ;; opening pair.
          (unless (looking-at "[ \t]*$")
            (newline))
          (goto-char :end-in)
          (beginning-of-line)
          ;; Add newline if there's anything but white-space before
          ;; and after the closing pair.
          (unless (looking-at "[ \t]*}[ \t]*$")
            (goto-char :end-in)
            (newline)))))))

(defun xen-php-handle-docstring (&rest _ignored)
  "Handle doc-strings for smartparens."
  (let ((line (save-excursion
                (forward-line)
                (thing-at-point 'line)))
        (content '())
        (jump-to nil)
        (bound nil))
    (cond
     ;; variable
     ((string-match (rx bol (0+ blank) (group "$" (1+ alnum))) line)
      (let ((var-name (match-string 1 line)))
        (setq content (concat " @var  " var-name " "))
        (setq jump-to (rx "@var "))
        ))
     ;; property
     ((string-match (rx bol (0+ blank) (or "private" "protected" "public" "var") (1+ " ") (group "$" (1+ alnum))) line)
      (let ((var-name (match-string 1 line))
            (type ""))
        ;; try to guess the type from the constructor
        (-when-let (constructor-args (xen-php-get-function-args "__construct" t))
          (setq type (or (cdr (assoc var-name constructor-args)) "")))
        (when type
          (setq type (xen-php-qualify-type type))
          )
        (setq content (list (concat "* @var " type "\n")))
        (setq jump-to (rx "@var "))
        ))
     ;; function
     ((string-match-p "function" line)
      (save-excursion
        (let ((args (save-excursion
                      (forward-line)
                      (xen-php-get-function-args nil t)))
              (params '()))
          (--each args
            (when (xen-php-should-insert-type-annotation (cdr it))
              (setq params (append params (list (format "* @param %s\n"
                                                        (mapconcat 'identity (list (xen-php-translate-type-annotation (cdr it))
                                                                                   (car it)) " ")))))))
          (when params
            (setq content (append content (list (mapconcat 'identity params ""))))))
        (let ((function-name (save-excursion
                               (forward-line)
                               (xen-php-get-function-name)))
              (return-type (save-excursion
                             (forward-line)
                             (xen-php-get-function-return-type))))
          (when (and (not (string= function-name "__construct"))
                     (xen-php-should-insert-type-annotation return-type))
            (setq content (append content (list (format "* @return %s\n" (xen-php-translate-type-annotation return-type))))))))
      (setq jump-to (rx "@" (or "param" "return") " "))
      )
     ;; class/interface
     ((string-match-p ".*class\\|interface" line)
      ;; (save-excursion (insert "\n"))
      ;; (insert "* ")
      (setq content (list "* \n"))
      (setq jump-to (rx bol (0+ blank) "* "))
      ))
    (if content
        (save-excursion
          (insert (if (listp content)
                      (concat "\n" (mapconcat 'identity content "*\n"))
                    content))
          (setq bound (point)))
      (insert "\n* ")
      (save-excursion
        (insert "\n"))
      )
    (when bound
      (re-search-forward jump-to bound t))
    )
  (let ((o (sp--get-active-overlay))
        (inhibit-message t))
    (indent-region (overlay-start o) (overlay-end o))))

(defun xen-php-get-function-name ()
  "Get the name of the function.

Point should be at the line containing `function'.
"
  (beginning-of-line)
  (search-forward "function ")
  (let ((start (point)))
    (search-forward "(")
    (buffer-substring-no-properties start (- (point) 1))))

(defun xen-php-get-function-args (&optional name types)
  "Return all arguments of php function.

Point should be at the line containing `function'.

If TYPES is non-nil, return a list of pairs with the variable
name in car and the type in cdr."
      (cl-block exit
        (save-excursion
          (when name
            (goto-char (point-min))
            (unless (search-forward (concat "function " name) nil t)
              (cl-return-from exit nil)))
          (let ((function-args (sp-get (sp-down-sexp)
                                 (buffer-substring-no-properties :beg :end)))
                (args nil))
            (save-match-data
              (with-temp-buffer
                (insert function-args)
                (goto-char (point-min))
                (if types
                    (while (re-search-forward "\\(?:\\([a-zA-Z0-9\\_]+\\) +\\)?\\(&?\\$.*?\\)[ \n\t,)]" nil t)
                      (push (cons (match-string 2) (match-string 1)) args))
                  (while (re-search-forward "\\(&?\\$.*?\\)[ \n\t,)]" nil t)
                    (push (match-string 1) args)))))
            (nreverse args)))))

(defun xen-php-qualify-type (type)
  "Get fully qualified name of TYPE.

Looks at use statements to determine FQN."
  (save-excursion
    (let ((case-fold-search nil))
      (goto-char (point-min))
      ;; Have to use rx-to-string and back-quoting in order to use the
      ;; ,type to inject the string. This is because the rx (eval)
      ;; form doesn't work with lexical-binding.
      (if (re-search-forward (rx-to-string `(seq bol "use " (group (1+ (in alpha digit "\\" "_")))
                                                 (or
                                                  (seq (group "\\" ,type) ";")
                                                  (seq " as " ,type ";")))) nil t)
          (concat "\\" (match-string 1) (when (match-string 2)
                                          (match-string 2)))
        type))))

(defun xen-php-should-insert-type-annotation (type)
  "Test if we should insert a TYPE annotation.

Only insert an docstring annotation if the TYPE and translated
type differ."
  (not (equal type (xen-php-translate-type-annotation type))))

(defun xen-php-translate-type-annotation (type)
  "Translate TYPE into string for annotation.

If the TYPE is array, return mixed[].  If the type is an object,
return as it is.  If type is nil, return an empty string."
      (cond
       ((equal type "array") "array<>")
       ((stringp type) type)
       ((null type) "")))

(defun xen-php-get-function-return-type (&optional name)
  "Return the return type of the function.

Point should be at the line containing `function'."
  (cl-block exit
    (save-excursion
      (when name
        (goto-char (point-min))
        (unless (search-forward (concat "function " name) nil t)
          (cl-return-from exit nil)))
      (goto-char (sp-get (sp-down-sexp) :end))
      (when (search-forward ":" (save-excursion (search-forward "{")) t)
        (when (re-search-forward "[a-zA-Z0-9\\_]+" nil t)
          (match-string-no-properties 0))))))

(defun xen-php-grab-class ()
  "Grab the PHP namespaced class at point.

Strips any leading backslash."
  (let (start end class)
    (skip-chars-backward "\\\\A-Za-z0-9_")
    (setq start (point))
    (skip-chars-forward "\\\\A-Za-z0-9_")
    (setq end (point))
    (when (> end start)
      (setq class (string-remove-prefix "\\" (buffer-substring-no-properties start end)))
      ;; Need at least one inline backslash in order to be a
      ;; namespaced class.
      (when (string-match-p (regexp-quote "\\") class)
        class))))

(defun xen-php-find-use-block ()
  "Find starting position of PHP use block."
  (interactive)
  (let ((inhibit-message t))
    (beginning-of-buffer)

    (while (looking-at "\\(<\\?php\\|declare\\|namespace\\|[[:space:]]*$\\)")
      (forward-line))

    (not (eobp))))

(defun xen-php-make-use ()
  "Add a PHP use statement for the fully-qualified name at point."
  (interactive)
  (let (class current-line)
    (save-excursion
      (setq class (xen-php-grab-class))
      (when class
        (when-let ((use-block (xen-php-find-use-block))
                   (line (concat "use " class ";\n")))
          (unless (looking-at "use ")
            (previous-line)
            (unless (looking-at "[[:space:]]*$"))
            (insert "\n"))
          (while (and (looking-at "use ")
                      (setq current-line (thing-at-point 'line t))
                      (and (not (equal line current-line))
                           (string> line current-line)))
            (forward-line))
          (unless (equal line current-line)
            (insert "\n")
            (previous-line)
            (xen-jitter-type (substring line 0 -1))))))
    (when class
      (let (start
            end
            (bare-class (last (split-string class "\\\\"))))
        (skip-chars-backward "\\\\A-Za-z0-9_")
        (setq start (point))
        (skip-chars-forward "\\\\A-Za-z0-9_")
        (setq end (point))
        (delete-region start end)
        (xen-jitter-type bare-class)))))

(provide 'xen-php)
;;; xen-php.el ends here
