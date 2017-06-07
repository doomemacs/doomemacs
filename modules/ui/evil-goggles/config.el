;;; ui/evil-goggles/config.el

(def-package! evil-goggles
  :when (featurep! :feature evil)
  :commands evil-goggles-mode
  :init
  (add-hook 'after-init-hook #'evil-goggles-mode)
  :config
  (setq evil-goggles-duration 0.1)
  (evil-goggles-use-diff-faces))
