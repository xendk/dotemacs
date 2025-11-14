;;; +rainbow-mode.el --- rainbow-mode additions      -*- lexical-binding: t; -*-

;;; Commentary:

;; Use overlay instead of text properties to override `hl-line' faces.
;; Taken from https://github.com/seagle0128/.emacs.d/blob/bf3d66248b883638d0b7ab584d5074f019fb555d/lisp/init-highlight.el#L148

;;; Code:

(declare-function rainbow-colorize-match "rainbow-mode")
(declare-function rainbow-turn-off "rainbow-mode")

(defun +rainbow-colorize-match (color &optional match)
  "Override `rainbow-colorize-match' to use overlays."
  (let* ((match (or match 0))
         (ov (make-overlay (match-beginning match) (match-end match))))
    (overlay-put ov 'ovrainbow t)
    ;; TODO look into using overlay priority to make rainbow overlay
    ;; win over the region.
    (overlay-put ov 'face `((:foreground ,(if (> 0.5 (rainbow-x-color-luminance color))
                                              "white" "black"))
                            (:background ,color)))))

(defun +rainbow-clear-overlays ()
  "Clear all rainbow overlays."
  (remove-overlays (point-min) (point-max) 'ovrainbow t))

(advice-add #'rainbow-colorize-match :override #'+rainbow-colorize-match)
(advice-add #'rainbow-turn-off :after #'+rainbow-clear-overlays)

(provide '+rainbow-mode)
;;; +rainbow-mode.el ends here
