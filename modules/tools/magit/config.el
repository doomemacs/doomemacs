;;; tools/magit/config.el -*- lexical-binding: t; -*-

(def-package! magit
  :defer t
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


(def-package! magit-blame :after git-timemachine)


(def-package! magithub
  :commands magithub-feature-autoinject
  :after magit
  :preface
  (setq magithub-dir (concat doom-etc-dir "magithub/"))
  :init
  (setq magithub-clone-default-directory "~/"
        magithub-preferred-remote-method 'clone_url)
  :config
  (magithub-feature-autoinject t))


(def-package! magit-gitflow
  :hook (magit-mode . turn-on-magit-gitflow))


(def-package! evil-magit
  :when (featurep! :feature evil)
  :after magit
  :init (setq evil-magit-state 'normal)
  :config
  (map! :map (magit-mode-map magit-blame-read-only-mode-map)
        doom-leader-key nil))
