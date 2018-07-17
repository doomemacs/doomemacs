;;; config.el --- description -*- lexical-binding: t; -*-

(def-package! nix-update
  :commands (nix-update-fetch))

(def-package! nix-repl
  :commands (nix-repl-show))

(def-package! helm-nixos-options
  :when (featurep! :completion helm)
  :commands (helm-nixos-options))

(after! nix-mode
  (map! :map nix-mode-map
        :localleader
        :n "f" #'nix-update-fetch
        :n "p" #'nix-format-buffer
        :n "r" #'nix-repl-show
        :n "s" #'nix-shell
        :n "b" #'nix-build
        :n "u" #'nix-unpack)

  (when (featurep! :completion helm)
    (map! :map nix-mode-map
          :localleader
          :n "o" #'helm-nixos-options))

  (when (featurep! :completion company)
    (set-company-backend! 'nix-mode 'company-nixos-options)))
