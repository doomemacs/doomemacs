;;; lang/nix/config.el -*- lexical-binding: t; -*-

(def-package! nix-mode
  :mode "\\.nix\\'"
  :config
  (set-company-backend! 'nix-mode 'company-nixos-options)

  (map! :map nix-mode-map
        :localleader
        :n "f" #'nix-update-fetch
        :n "p" #'nix-format-buffer
        :n "r" #'nix-repl-show
        :n "s" #'nix-shell
        :n "b" #'nix-build
        :n "u" #'nix-unpack
        (:when (featurep! :completion helm)
          :n "o" #'helm-nixos-options)))

(def-package! nix-drv-mode
  :mode "\\.drv\\'")

(def-package! nix-update
  :commands nix-update-fetch)

(def-package! nix-repl
  :commands nix-repl-show)
