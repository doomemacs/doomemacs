;;; ui/doom/config.el -*- lexical-binding: t; -*-

;;;###package pos-tip
(setq pos-tip-internal-border-width 6
      pos-tip-border-width 1)


(use-package! doom-themes
  ;; improve integration w/ org-mode
  :hook (doom-load-theme . doom-themes-org-config)
  :init (setq doom-theme 'doom-one)
  ;; more Atom-esque file icons for neotree/treemacs
  (when (modulep! :ui neotree)
    (add-hook 'doom-load-theme-hook #'doom-themes-neotree-config)
    (setq doom-themes-neotree-enable-variable-pitch t
          doom-themes-neotree-file-icons 'simple
          doom-themes-neotree-line-spacing 2))
  (when (modulep! :ui treemacs)
    (add-hook 'doom-load-theme-hook #'doom-themes-treemacs-config)))


(use-package! solaire-mode
  :hook (doom-load-theme . solaire-global-mode)
  :hook (+popup-buffer-mode . turn-on-solaire-mode))
