;;; ui/unicode/config.el -*- lexical-binding: t; -*-

(def-package! unicode-fonts
  :demand t
  :init
  (setq-default bidi-display-reordering t
                doom-unicode-font nil)
  :config
  ;; NOTE will impact startup time on first run
  (unicode-fonts-setup))
