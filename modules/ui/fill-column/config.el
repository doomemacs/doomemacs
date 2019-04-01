;;; ui/fill-column/config.el -*- lexical-binding: t; -*-

(def-package! hl-fill-column
  :hook ((text-mode prog-mode conf-mode) . hl-fill-column-mode))
