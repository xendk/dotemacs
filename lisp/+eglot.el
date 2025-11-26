;;; +eglot.el --- eglot additions                    -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(declare-function eglot--lookup-mode "eglot")

(defun +eglot ()
  "Enable eglot in supported buffers."
  (require 'eglot)
  (when (eglot--lookup-mode major-mode)
    (eglot-ensure)))

(provide '+eglot)
;;; +eglot.el ends here
