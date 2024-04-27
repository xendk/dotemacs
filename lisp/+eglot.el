;;; +eglot.el --- eglot additions                    -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun +eglot ()
  "Enable eglot in supported buffers."
  (require 'eglot)
  (when (eglot--lookup-mode major-mode)
    (eglot-ensure)))

(provide '+eglot)
;;; +eglot.el ends here
