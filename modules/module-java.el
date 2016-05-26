;;; module-java.el

(after! c-initialization
  (def-docset! java-mode "java,javafx,grails,groovy,playjava,spring,cvj,processing,javadoc"))

(defvar eclim-eclipse-dirs '("/Applications/eclipse"))
(defvar eclim-executable     "/Applications/eclipse/eclim")
(use-package eclim
  :functions (eclim--project-dir eclim--project-name)
  :commands (eclim-mode global-eclim-mode)
  :preface
  :when (file-exists-p eclim-executable)
  :init (add-hook 'java-mode-hook 'eclim-mode)
  :config
  ;; (require 'eclim-ant)
  ;; (require 'eclim-maven)
  (require 'eclim-problems)
  (require 'eclim-project)
  (require 'eclimd)
  (setq help-at-pt-display-when-idle t)
  (setq help-at-pt-timer-delay 0.1)
  (help-at-pt-set-timer)

  (map! :map java-mode-map :m "gd" 'eclim-java-find-declaration))

(use-package company-emacs-eclim
  :after eclim
  :config (company-emacs-eclim-setup))

(use-package android-mode
  :commands android-mode
  :init
  (add-hook! (java-mode groovy-mode nxml-mode) 'doom|android-mode-enable-maybe)
  :config
  (def-yas-mode! 'android-mode)
  (after! company-dict
    (push 'android-mode company-dict-minor-mode-list)))

(use-package groovy-mode
  :mode "\\.g\\(radle\\|vy\\|roovy\\)$"
  :config
  (after! quickrun
    (push '("\\.gvy$" . "groovy") quickrun-file-alist)))

(provide 'module-java)
;;; module-java.el ends here
