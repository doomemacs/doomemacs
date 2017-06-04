;;; ui/evil-goggles/config.el

(def-package! evil-goggles :demand t
  :config
  (evil-goggles-mode +1)
  (evil-goggles-use-diff-faces))
