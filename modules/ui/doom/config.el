;;; ui/doom/config.el -*- lexical-binding: t; -*-

;;;###package pos-tip
(setq pos-tip-internal-border-width 6
      pos-tip-border-width 1)


(use-package! doom-themes
  :defer t
  :init
  (setq doom-theme 'doom-one)
  ;; improve integration w/ org-mode
  (add-hook 'doom-load-theme-hook #'doom-themes-org-config)
  ;; more Atom-esque file icons for neotree/treemacs
  (when (featurep! :ui neotree)
    (add-hook 'doom-load-theme-hook #'doom-themes-neotree-config)
    (setq doom-themes-neotree-enable-variable-pitch t
          doom-themes-neotree-file-icons 'simple
          doom-themes-neotree-line-spacing 2))
  (when (featurep! :ui treemacs)
    (add-hook 'doom-load-theme-hook #'doom-themes-treemacs-config)))


(use-package! solaire-mode
  :when (or (daemonp) (display-graphic-p))
  :hook (doom-load-theme . solaire-global-mode))
