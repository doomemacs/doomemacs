;;; ui/modeline/autoload/modeline.el -*- lexical-binding: t; -*-

;;;###autodef
(defalias 'def-modeline-format! #'doom-modeline-def-modeline)

;;;###autodef
(defalias 'def-modeline-segment! #'doom-modeline-def-segment)

;;;###autodef
(defalias 'set-modeline! #'doom-modeline-set-modeline)


(defvar +modeline--old-bar-height nil)
;;;###autoload
(defun +modeline-resize-for-font-h ()
  "Adjust the modeline's height when the font size is changed by
`doom/increase-font-size' or `doom/decrease-font-size'.

Meant for `doom-change-font-size-hook'."
  (unless +modeline--old-bar-height
    (setq +modeline--old-bar-height doom-modeline-height))
  (let ((default-height +modeline--old-bar-height)
        (scale (or (frame-parameter nil 'font-scale) 0)))
    (setq doom-modeline-height
          (if (> scale 0)
              (+ default-height (* scale doom-font-increment))
            default-height))))

;;;###autoload
(defun +modeline-update-env-in-all-windows-h (&rest _)
  "Update version strings in all buffers."
  (dolist (window (window-list))
    (with-selected-window window
      (doom-modeline-update-env)
      (force-mode-line-update))))

;;;###autoload
(defun +modeline-clear-env-in-all-windows-h (&rest _)
  "Blank out version strings in all buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (setq doom-modeline-env--version
            (bound-and-true-p doom-modeline-load-string))))
  (force-mode-line-update t))
