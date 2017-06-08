;;; ui/evil-goggles/config.el -*- lexical-binding: t; -*-

(def-package! evil-goggles
  :when (featurep! :feature evil)
  :commands evil-goggles-mode
  :init
  (setq evil-goggles-duration 0.1
        evil-goggles-enable-delete nil)
  (add-hook 'emacs-startup-hook #'evil-goggles-mode t))
