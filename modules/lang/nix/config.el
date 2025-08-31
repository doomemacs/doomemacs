;;; lang/nix/config.el -*- lexical-binding: t; -*-

(after! tramp
  (add-to-list 'tramp-remote-path "/run/current-system/sw/bin"))


;;
;;; Plugins

(add-to-list 'auto-mode-alist
             (cons "/flake\\.lock\\'"
                   (if (modulep! :lang json)
                       'json-mode
                     'js-mode)))


(defun +nix-common-config (mode)
  (set-repl-handler! mode #'+nix/open-repl)
  (set-company-backend! mode 'company-nixos-options)
  (set-lookup-handlers! mode
    :documentation '(+nix/lookup-option :async t))
  (set-popup-rule! "^\\*nixos-options-doc\\*$" :ttl 0 :quit t)

  (when (modulep! +lsp)
    (add-hook (intern (format "%s-local-vars-hook" mode)) #'lsp! 'append))

  (map! :localleader
        :map ,(intern (format "%s-map" mode))
        "f" #'nix-update-fetch
        "p" #'nix-format-buffer
        "r" #'nix-repl-show
        "s" #'nix-shell
        "b" #'nix-build
        "u" #'nix-unpack
        "o" #'+nix/lookup-option))


(use-package! nix-mode
  :interpreter ("\\(?:cached-\\)?nix-shell" . +nix-shell-init-mode)
  :mode "\\.nix\\'"
  :config
  (+nix-common-config 'nix-mode))


(use-package! nix-ts-mode
  :when (modulep! +tree-sitter)
  :when (fboundp 'treesit-available-p)
  :defer t
  :init
  (set-tree-sitter! 'nix-mode 'nix-ts-mode
    '((nix :url "https://github.com/nix-community/tree-sitter-nix")))
  :config
  (+nix-common-config 'nix-ts-mode))


(use-package! company-nixos-options
  :defer t
  :init
  ;; Fix #3927: disable idle completion because `company-nixos-options' is
  ;; dreadfully slow. It can still be invoked manually..
  (setq-hook! '(nix-mode-hook nix-ts-mode-hook) company-idle-delay nil))


(use-package! nix-update
  :commands nix-update-fetch)


(use-package! nix-repl
  :commands nix-repl-show)
