;;; ui/minimap/config.el -*- lexical-binding: t; -*-

(use-package! minimap
  :hook doom-load-theme-hook
  :config
  (setq minimap-window-location 'right
        minimap-update-delay 0
        minimap-width-fraction 0.09
        minimap-minimum-width 15)
  (custom-set-faces!
    `(minimap-current-line-face
      :background ,(doom-color 'selection))
    `(minimap-active-region-background
      :background ,(doom-color 'vertical-bar))))
