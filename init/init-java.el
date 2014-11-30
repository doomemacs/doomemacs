(provide 'init-java)

(use-package eclim
  :commands (eclim-mode global-eclim-mode)
  :init
  (progn
    (setq eclim-eclipse-dirs '("/Applications/eclipse")
          eclim-executable "/Applications/eclipse/eclim")
    (add-hook 'java-mode-hook 'eclim-mode))
  :config
  (progn
    ;; (use-package eclim-ant)
    ;; (use-package eclim-maven)
    (use-package eclim-problems)
    (use-package eclim-project)
    (use-package eclimd)

    (require 'ac-emacs-eclim-source)
    (add-hook! 'java-mode-hook
               (setq ac-sources '(ac-source-emacs-eclim ac-source-yasnippet ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers)))

    (setq help-at-pt-display-when-idle t)
    (setq help-at-pt-timer-delay 0.1)
    (help-at-pt-set-timer)

    (bind 'motion java-mode-map "gd" 'eclim-java-find-declaration)))

(use-package groovy-mode :mode "\\.gradle$"
  :config
  (add-to-list 'ac-modes 'groovy-mode))
