;;; lang/nix/config.el -*- lexical-binding: t; -*-

(after! tramp
  (add-to-list 'tramp-remote-path "/run/current-system/sw/bin"))


;;
;;; Plugins

(use-package! nix-mode
  :interpreter ("\\(?:cached-\\)?nix-shell" . +nix-shell-init-mode)
  :mode "\\.nix\\'"
  :init
  ;; Treat flake.lock files as json. Fall back to js-mode because it's faster
  ;; than js2-mode, and its extra features aren't needed there.
  (add-to-list 'auto-mode-alist
               (cons "/flake\\.lock\\'"
                     (if (featurep! :lang json)
                         'json-mode
                       'js-mode)))
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
