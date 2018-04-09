;;; tools/magit/config.el -*- lexical-binding: t; -*-

(def-package! magit
  :defer t
  :init
  (load "magit-autoloads" nil t)
  :config
  (setq magit-completing-read-function
        (if (featurep! :completion ivy)
            #'ivy-completing-read
          #'magit-builtin-completing-read)
        magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))

  (set! :popup "^\\(?:\\*magit\\|magit:\\)" :ignore)
  ;; no mode-line in magit popups
  (add-hook 'magit-popup-mode-hook #'hide-mode-line-mode)
  ;; Clean up after magit by properly killing buffers
  (map! :map magit-status-mode-map [remap magit-mode-bury-buffer] #'+magit/quit))


(def-package! magit-blame
  :commands magit-blame
  :after git-timemachine)


(def-package! magithub
  :commands (magithub-clone magithub-feature-autoinject)
  :after magit
  :preface
  (setq magithub-dir (concat doom-etc-dir "magithub/"))
  :init
  (setq magithub-clone-default-directory "~/"
        magithub-preferred-remote-method 'clone_url)
  :config
  (load "magithub-autoloads" nil t)
  (magithub-feature-autoinject t))


(def-package! evil-magit
  :when (featurep! :feature evil)
  :after magit
  :config
  (setq evil-magit-state 'normal)

  ;; Switch to emacs state only while in `magit-blame-mode', then back when
  ;; its done (since it's a minor-mode).
  (add-hook! 'magit-blame-mode-hook
    (evil-local-mode (if magit-blame-mode -1 +1))))
