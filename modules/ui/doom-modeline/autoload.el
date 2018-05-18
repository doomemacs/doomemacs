;;; ui/doom-modeline/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +doom-modeline|resize-for-big-font ()
  "Adjust the modeline's height when `doom-big-font-mode' is enabled. This was
made to be added to `doom-big-font-mode-hook'."
  (let ((default-height (default-value '+doom-modeline-height)))
    (if doom-big-font-mode
        (let* ((font-size (font-get doom-font :size))
               (big-size (font-get doom-big-font :size))
               (ratio (/ (float big-size) font-size)))
          (setq +doom-modeline-height (ceiling (* default-height ratio))))
      (setq +doom-modeline-height default-height))
    ;; already has a variable watcher in Emacs 26+
    (unless EMACS26+ (+doom-modeline|refresh-bars))))

