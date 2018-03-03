;;; ui/doom-modeline/autoload.el -*- lexical-binding: t; -*-

(defvar +doom-modeline--old-height nil)

;;;###autoload
(defun +doom-modeline|resize-for-big-font ()
  "Adjust the modeline's height when `doom-big-font-mode' is enabled. This was
made to be added to `doom-big-font-mode-hook'."
  (if doom-big-font-mode
      (let* ((font-size (font-get doom-font :size))
             (big-size (font-get doom-big-font :size))
             (ratio (/ (float big-size) font-size)))
        (setq +doom-modeline--old-height +doom-modeline-height
              +doom-modeline-height (ceiling (* +doom-modeline--old-height ratio))))
    (setq +doom-modeline-height +doom-modeline--old-height))
  (+doom-modeline|init))

