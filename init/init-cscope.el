(provide 'init-cscope)

(use-package xcscope
  :config
  (progn
    (cscope-setup)
    (add-hook 'ruby-mode-hook (function cscope-minor-mode))

    (push '("*cscope*" :position bottom) popwin:special-display-config)))
