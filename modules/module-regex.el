;;; module-regex.el

(use-package re-builder
  :commands (re-builder reb-mode-buffer-p)
  :config
  (require 'pcre2el)
  (setq reb-re-syntax 'pcre)
  (map! :map rxt-help-mode-map :n [escape] 'kill-buffer-and-window)

  (add-hook! reb-mode 'narf|reb-cleanup)
  (evil-set-initial-state 'reb-mode 'insert)
  (map! :map reb-mode-map
        :n "C-g"        'reb-quit
        :n [escape]     'reb-quit
        :n [backtab]    'reb-change-syntax))

(provide 'module-regex)
;;; module-regex.el ends here
