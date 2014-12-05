(provide 'init-cscope)

(use-package xcscope
  :init (cscope-setup)
  :config (push '("*cscope*" :position bottom) popwin:special-display-config))

(add-hook 'ruby-mode-hook (function cscope-minor-mode))
