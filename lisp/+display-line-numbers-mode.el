;;; +display-line-numbers-mode.el --- display-line-numbers-mode additions  -*- lexical-binding: t; -*-
;;;
;;; Commentary:

;;; Code:

(defun +display-line-numbers-mode ()
  "Enable line numbers in file-visiting buffers."
  (when (buffer-file-name (buffer-base-buffer))
    (display-line-numbers-mode 1)))

(provide '+display-line-numbers-mode)
;;; +display-line-numbers-mode.el ends here
