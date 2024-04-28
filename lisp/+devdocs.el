;;; +devdocs.el --- devdocs additions                -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; A tag renderer that handles the view-source element so it doesn't
;; stick to the return typehint.
(defun +crystal-tag-a (dom)
  (let ((class (dom-attr dom 'class)))
    (when (string= class "view-source")
      (insert " ["))
    (shr-tag-a dom)
    (when (string= class "view-source")
      (insert "]"))))

(provide '+devdocs)
;;; +devdocs.el ends here
