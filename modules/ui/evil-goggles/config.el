;;; ui/evil-goggles/config.el -*- lexical-binding: t; -*-

(def-package! evil-goggles
  :when (featurep! :feature evil)
  :hook (doom-post-init . evil-goggles-mode)
  :init
  (setq evil-goggles-duration 0.05
        evil-goggles-pulse nil ; too slow
        evil-goggles-enable-delete nil))
