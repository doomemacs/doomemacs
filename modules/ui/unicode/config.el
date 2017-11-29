;;; ui/unicode/config.el -*- lexical-binding: t; -*-

(def-package! unicode-fonts
  :init
  (setq-default bidi-display-reordering t
                doom-unicode-font nil)

  (defun +unicode|init-fonts (&optional frame)
    "Initialize `unicode-fonts', if in a GUI session."
    (when (and frame (display-graphic-p frame))
      (with-selected-frame frame
        (require 'unicode-fonts)
        ;; NOTE will impact startup time on first run
        (unicode-fonts-setup))))

  (add-hook! 'after-init-hook
    (if initial-window-system
        (+unicode|init-fonts)
      (add-hook 'after-make-frame-functions #'+unicode|init-fonts))))
