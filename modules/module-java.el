;;; module-java.el --- the poster child for carpal tunnel

(define-docset! java-mode "java,droid,javafx,grails,groovy,playjava,spring,cvj,processing,javadoc")

(use-package eclim
  :functions (eclim--project-dir eclim--project-name)
  :commands (eclim-mode global-eclim-mode)
  :init
  (setq eclim-eclipse-dirs '("/Applications/eclipse")
        eclim-executable     "/Applications/eclipse/eclim")
  (when (file-exists-p eclim-executable)
    (add-hook 'java-mode-hook 'eclim-mode))
  :config
  ;; (require 'eclim-ant)
  ;; (require 'eclim-maven)
  (require 'eclim-problems)
  (require 'eclim-project)
  (require 'eclimd)
  (setq help-at-pt-display-when-idle t)
  (setq help-at-pt-timer-delay 0.1)
  (help-at-pt-set-timer)

  (use-package company-emacs-eclim
    :functions company-emacs-eclim-setup
    :config (company-emacs-eclim-setup))

  (map! :map java-mode-map :m "gd" 'eclim-java-find-declaration))

(use-package android-mode
  :commands android-mode
  :init
  (after! company-dict (add-to-list 'company-dict-minor-mode-list 'android-mode))
  (add-hook! (java-mode groovy-mode nxml-mode) 'narf|android-mode-enable-maybe)
  (add-hook! android-mode (add-yas-minor-mode! 'android-mode)))

(use-package groovy-mode
  :functions (is-groovy-mode)
  :mode "\\.g\\(radle\\|vy\\|roovy\\)$")

(use-package scala-mode2
  :mode ("\\.s\\(cala\\|bt\\)$" . scala-mode))

(provide 'module-java)
;;; module-java.el ends here
