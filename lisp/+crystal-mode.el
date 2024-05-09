;;; +crystal-mode.el --- crystal-mode additions      -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; A tag renderer for devdocs that handles the view-source element so
;; it doesn't stick to the return typehint.
(defun +crystal-mode-tag-a (dom)
  (let ((class (dom-attr dom 'class)))
    (when (string= class "view-source")
      (insert " ["))
    (shr-tag-a dom)
    (when (string= class "view-source")
      (insert "]"))))


(provide '+crystal-mode)
;;; +crystal-mode.el ends here
