(use-package eclim
  :commands (eclim-mode global-eclim-mode)
  :config
  (progn
    (setq eclim-eclipse-dirs '("/Applications/eclipse")
          eclim-executable "/Applications/eclipse/eclim")
    (add-hook 'java-mode-hook 'eclim-mode)

    ;; (use-package eclim-ant)
    ;; (use-package eclim-maven)
    (use-package eclim-problems)
    (use-package eclim-project)
    (use-package eclimd)

    (setq help-at-pt-display-when-idle t)
    (setq help-at-pt-timer-delay 0.1)
    (help-at-pt-set-timer)

    (after "company"
      (use-package company-emacs-eclim
        :init (company-emacs-eclim-setup)))

    (after "auto-complete"
      (add-hook! 'java-mode-hook
                 (setq ac-sources '(ac-source-emacs-eclim
                                    ac-source-yasnippet
                                    ac-source-abbrev
                                    ac-source-dictionary
                                    ac-source-words-in-same-mode-buffers))))

    (bind 'motion java-mode-map "gd" 'eclim-java-find-declaration)))

(use-package groovy-mode
  :mode "\\.gradle$"
  :config (after "auto-complete" (add-to-list 'ac-modes 'groovy-mode)))

(sp-with-modes '(java-mode)
    (sp-local-pair "/**" "*/" :post-handlers '(("* ||\n[i]" "RET"))))


(provide 'init-java)
;;; init-java.el ends here
