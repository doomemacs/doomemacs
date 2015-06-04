(defun narf/java-project-package ()
  (if (eq major-mode 'java-mode)
    (s-chop-suffix "." (s-replace "/" "." (f-dirname (f-relative (buffer-file-name)
                                                                 (concat (narf/project-root) "/src/")))))
    ""))

(defun narf/java-class-name ()
  (if (eq major-mode 'java-mode)
      (f-no-ext (f-base (buffer-file-name)))
    ""))

(use-package eclim
  :functions (eclim--project-dir eclim--project-name)
  :commands (eclim-mode global-eclim-mode)
  :init
  (progn
    (setq eclim-eclipse-dirs '("/Applications/eclipse")
          eclim-executable     "/Applications/eclipse/eclim")
    (when (file-exists-p eclim-executable)
      (add-hook 'java-mode-hook 'eclim-mode)))
  :config
  (progn
    ;; (use-package eclim-ant)
    ;; (use-package eclim-maven)
    (use-package eclim-problems)
    (use-package eclim-project)
    (use-package eclimd)

    (setq help-at-pt-display-when-idle t)
    (setq help-at-pt-timer-delay 0.1)
    (help-at-pt-set-timer)

    (push "*eclim: problems*" winner-boring-buffers)

    (after "company"
      (use-package company-emacs-eclim
        :config (company-emacs-eclim-setup)))

    (bind :motion :map java-mode-map "gd" 'eclim-java-find-declaration)))

(use-package android-mode
  :commands android-mode
  :init
  (progn
    ;; yasnippet defuns
    (defun android-mode-is-layout-file ()
      (and android-mode
           (eq major-mode 'nxml-mode)
           (string-equal (file-name-base (directory-file-name default-directory)) "layout")))

    (defun android-mode-in-tags (&rest tags)
      (-contains? tags (android-mode-tag-name)))

    (defun android-mode-tag-name ()
      (save-excursion
        (let (beg end)
          (nxml-backward-up-element)
          (evil-forward-word-begin)
          (setq beg (point))
          (evil-forward-WORD-end)
          (setq end (1+ (point)))
          (buffer-substring-no-properties beg end))))

    (defun narf|android-mode-enable-maybe ()
      (let ((root (narf/project-root)))
        (when (or (narf/project-has-files "local.properties" root)
                  (narf/project-has-files "AndroidManifest.xml" root)
                  (narf/project-has-files "src/main/AndroidManifest.xml" root))
          (android-mode +1)
          (narf/set-build-command "./gradlew %s" "build.gradle"))))

    (after "company" (add-to-list 'company-dictionary-major-minor-modes 'android-mode))
    (add-hook 'java-mode-hook     'narf|android-mode-enable-maybe)
    (add-hook 'groovy-mode-hook   'narf|android-mode-enable-maybe)
    (add-hook 'nxml-mode-hook     'narf|android-mode-enable-maybe)
    (add-hook! 'android-mode-hook (narf/init-yas-mode 'android-mode))))

(use-package groovy-mode
  :functions (is-groovy-mode)
  :mode "\\.gradle$")


(provide 'init-java)
;;; init-java.el ends here
