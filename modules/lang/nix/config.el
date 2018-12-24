;;; lang/nix/config.el -*- lexical-binding: t; -*-

(def-package! nix-mode
  :mode "\\.nix\\'"
  :config
  (set-company-backend! 'nix-mode 'company-nixos-options)

  (map! :localleader
        :map nix-mode-map
        "f" #'nix-update-fetch
        "p" #'nix-format-buffer
        "r" #'nix-repl-show
        "s" #'nix-shell
        "b" #'nix-build
        "u" #'nix-unpack
        (:when (featurep! :completion helm)
          "o" #'helm-nixos-options)))

(def-package! nix-drv-mode
  :mode "\\.drv\\'")

(def-package! nix-update
  :commands nix-update-fetch)

(def-package! nix-repl
  :commands nix-repl-show)
