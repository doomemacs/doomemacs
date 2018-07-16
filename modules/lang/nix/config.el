;;; config.el --- description -*- lexical-binding: t; -*-

(def-package! company-nixos-options
  :when (featurep! :completion company)
  :after nix-mode
  :config
  (set-company-backend! 'nix-mode 'company-nixos-options))

(def-package! nix-update
  :after nix-mode
  :commands (nix-update-fetch))

(def-package! nix-repl
  :after nix-mode
  :commands (nix-repl-show))

(def-package! nix-mode
  :config
  (map! :map nix-mode-map
        :localleader
        :n "f" #'nix-update-fetch
        :n "p" #'nix-format-buffer
        :n "r" #'nix-repl-show
        :n "s" #'nix-shell
        :n "b" #'nix-build
        :n "u" #'nix-unpack))
