(defun my-java-project-package ()
  (if (eq major-mode 'java-mode)
    (s-chop-suffix "." (s-replace "/" "." (f-dirname (f-relative (buffer-file-name)
                                                                 (concat (my--project-root) "src/")))))
    ""))

(defun my-java-class-name ()
  (if (eq major-mode 'java-mode)
      (f-no-ext (f-base (buffer-file-name)))
    ""))

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
        :config (company-emacs-eclim-setup)))

    (bind 'motion java-mode-map "gd" 'eclim-java-find-declaration)))

(use-package android-mode
  :defer t
  :init
  (add-hook! 'java-mode-hook
             (when (f-exists? (concat (my--project-root) "AndroidManifest.xml"))
               (android-mode +1))))

(use-package groovy-mode :mode "\\.gradle$")

(provide 'init-java)
;;; init-java.el ends here
