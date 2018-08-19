;;; ui/modeline/autoload/modeline.el -*- lexical-binding: t; -*-

;; (defvar +modeline--old-bar-height nil)
;; ;;;###autoload
;; (defun +modeline|resize-for-big-font ()
;;   "Adjust the modeline's height when `doom-big-font-mode' is enabled. This was
;; made to be added to `doom-big-font-mode-hook'."
;;   (unless +modeline--old-bar-height
;;     (setq +modeline--old-bar-height +doom-modeline-height))
;;   (let ((default-height +modeline--old-bar-height))
;;     (if doom-big-font-mode
;;         (let* ((font-size (font-get doom-font :size))
;;                (big-size (font-get doom-big-font :size))
;;                (ratio (/ (float big-size) font-size)))
;;           (setq +doom-modeline-height (ceiling (* default-height ratio 0.75))))
;;       (setq +doom-modeline-height default-height))
;;     ;; already has a variable watcher in Emacs 26+
;;     (unless EMACS26+ (+doom-modeline|refresh-bars))))

