;;; lang/nix/config.el -*- lexical-binding: t; -*-

(after! tramp
  (add-to-list 'tramp-remote-path "/run/current-system/sw/bin"))


;;
;;; Plugins

(use-package! nix-mode
  :interpreter ("\\(?:cached-\\)?nix-shell" . +nix-shell-init-mode)
  :mode "\\.nix\\'"
  :config
  (set-repl-handler! 'nix-mode #'+nix/open-repl)
  (set-company-backend! 'nix-mode 'company-nixos-options)
  (set-lookup-handlers! 'nix-mode
    :documentation '(+nix/lookup-option :async t))
  (set-popup-rule! "^\\*nixos-options-doc\\*$" :ttl 0 :quit t)

  ;; Fix #3927: disable idle completion because `company-nixos-options' is
  ;; dreadfully slow. It can still be invoked manually..
  (setq-hook! 'nix-mode-hook company-idle-delay nil)

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
