;;; module-regex.el

(use-package pcre2el
  :functions (rxt--re-builder-switch-pcre-mode)
  :after re-builder
  :config
  (setq reb-re-syntax 'pcre)
  (bind! :map rxt-help-mode-map :n [escape] 'kill-buffer-and-window))

(use-package re-builder
  :commands (re-builder reb-mode-buffer-p)
  :config
  (add-hook! reb-mode 'narf|reb-cleanup)
  (evil-set-initial-state 'reb-mode 'insert)
  (bind! :map reb-mode-map
         :n "C-g"        'reb-quit
         :n [escape]     'reb-quit
         :n [backtab]    'reb-change-syntax))

(provide 'module-regex)
;;; module-regex.el ends here
