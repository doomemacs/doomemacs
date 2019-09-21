;;; lang/nix/config.el -*- lexical-binding: t; -*-

(use-package! nix-mode
  :mode "\\.nix\\'"
  :config
  (set-company-backend! 'nix-mode 'company-nixos-options)
  (set-lookup-handlers! 'nix-mode
    :documentation '(+nix/lookup-option :async t))

  (map! :localleader
        :map nix-mode-map
        "f" #'nix-update-fetch
        "p" #'nix-format-buffer
        "r" #'nix-repl-show
        "s" #'nix-shell
        "b" #'nix-build
        "u" #'nix-unpack
        "o" #'+nix/lookup-option))

(use-package! nix-drv-mode
  :mode "\\.drv\\'")

(use-package! nix-update
  :commands nix-update-fetch)

(use-package! nix-repl
  :commands nix-repl-show)
