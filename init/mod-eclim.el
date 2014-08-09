(use-package eclim :ensure emacs-eclim
  :commands (eclim-mode global-eclim-mode)
  :init
  (progn
    (setq eclim-eclipse-dirs '("~/.opt/eclipse")
          eclim-executable "~/.opt/eclipse/eclim")
    (add-hook 'java-mode-hook 'eclim-mode))
  :config
  (progn
    ;; (use-package eclim-ant)
    ;; (use-package eclim-maven)
    (use-package eclim-problems)
    (use-package eclim-project)
    (use-package eclimd)

    (require 'ac-emacs-eclim-source)
    (ac-emacs-eclim-java-setup)

    (setq help-at-pt-display-when-idle t)
    (setq help-at-pt-timer-delay 0.1)
    (help-at-pt-set-timer)
    ))

;;
(provide 'mod-eclim)
