;;; ui/unicode/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(add-hook 'doom-post-init-hook #'+unicode|init-fonts)

;;;###autoload
(defun +unicode|init-fonts ()
  "Set up `unicode-fonts' to eventually run; accomodating the daemon, if
necessary."
  (setq-default bidi-display-reordering t
                doom-unicode-font nil)
  (if initial-window-system
      (+unicode|setup-fonts (selected-frame))
    (add-hook 'after-make-frame-functions #'+unicode|setup-fonts)))

;;;###autoload
(defun +unicode|setup-fonts (&optional frame)
  "Initialize `unicode-fonts', if in a GUI session."
  (when (and frame (display-graphic-p frame))
    (with-selected-frame frame
      (require 'unicode-fonts)
      ;; NOTE will impact startup time on first run
      (unicode-fonts-setup))))
