;;; +eglot.el --- eglot additions                    -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(declare-function eglot--lookup-mode "eglot")

(defun +eglot ()
  "Enable eglot in supported buffers."
  (require 'eglot)

  ;; Gemini thinks that crystalline returns severities as string
  ;; instead of numbers. This advice fixes it. Seems legit, but other
  ;; crystalline users doesn't seem to have a problem, so needs
  ;; investigating.
  (defun +eglot-fix-severity (orig-fn server method &rest args)
    "Fix non-compliant diagnostic severities sent as strings by some LSPs."
    (when (eq method 'textDocument/publishDiagnostics)
      (let ((diagnostics (plist-get args :diagnostics)))
        (when (vectorp diagnostics)
          ;; Loop through the vector of diagnostics and sanitize the :severity
          (cl-loop for i from 0 below (length diagnostics)
                   do (let* ((diag (aref diagnostics i))
                             (sev (plist-get diag :severity)))
                        (when (stringp sev)
                          ;; Overwrite the vector element with the sanitized plist
                          (aset diagnostics i
                                (plist-put diag :severity
                                           (pcase (downcase sev)
                                             ("error" 1)
                                             ("warning" 2)
                                             ("information" 3)
                                             ("info" 3)
                                             ("hint" 4)
                                             (_ 1))))))))))
    (apply orig-fn server method args))
  (advice-add 'eglot-handle-notification :around #'+eglot-fix-severity)

  (when (eglot--lookup-mode major-mode)
    (eglot-ensure)))

(provide '+eglot)
;;; +eglot.el ends here
