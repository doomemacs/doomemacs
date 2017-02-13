;;; feature/spellcheck/config.el

(@def-package flyspell ; built-in
  :commands flyspell-mode
  :init (@add-hook text-mode 'flyspell-mode))


(@def-package flyspell-correct
  :commands (flyspell-correct-word-generic
             flyspell-correct-previous-word-generic))


(@def-package flyspell-correct-popup
  :after flyspell-correct
  :config
  (setq flyspell-popup-correct-delay 0.8)
  (add-hook 'flyspell-mode-hook 'flyspell-popup-auto-correct-mode))


(@def-package flyspell-correct-ivy
  :when (@featurep :completion ivy)
  :after flyspell-correct)

(@def-package flyspell-correct-helm
  :when (@featurep :completion helm)
  :after flyspell-correct)

(@def-package flyspell-correct-ido
  :when (@featurep :emacs ido)
  :after flyspell-correct)

