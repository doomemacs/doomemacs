;;; ui/dimmer/config.el -*- lexical-binding: t; -*-

(def-package! dimmer
  :config
  (setq dimmer-exclusion-regexp "\*"
        dimmer-fraction 0.50)
  (dimmer-mode 1)
  (remove-hook 'focus-in-hook 'dimmer-config-change-hook)
  (remove-hook 'focus-out-hook 'dimmer-dim-all))
