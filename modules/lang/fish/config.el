;;; lang/fish/config.el -*- lexical-binding: t; -*-

(def-package! fish-mode
  :mode "\\.fish$"
  :interpreter "fish"
  :config
  (add-hook! fish-mode
    (add-hook 'before-save-hook #'fish_indent-before-save)))

