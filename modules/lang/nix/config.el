;;; config.el --- description -*- lexical-binding: t; -*-

(def-package! company-nixos-options
  :when (featurep! :completion company)
  :after nix-mode
  :config
  (set-company-backend! 'nix-mode 'company-nixos-options))

(def-package! nix-update
  :after nix-mode
  :commands (nix-update-fetch))
