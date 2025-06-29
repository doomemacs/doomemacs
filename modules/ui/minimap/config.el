;;; ui/minimap/config.el -*- lexical-binding: t; -*-

(use-package! demap
  :hook (demap-minimap-window-set-hook . hide-mode-line-mode))
