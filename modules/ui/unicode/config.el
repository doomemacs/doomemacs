;;; ui/unicode/config.el -*- lexical-binding: t; -*-

(setq-default bidi-display-reordering t)

(def-package! unicode-fonts
  :demand t
  :config
  ;; NOTE will impact startup time on first run
  (unicode-fonts-setup))
