;;; lang/rust/config.el -*- lexical-binding: t; -*-

;;
;; Plugins
;;

(def-package! rust-mode
  :mode "\\.rs$"
  :config
  (set! :env "RUST_SRC_PATH")
  (set! :docset 'rust-mode "Rust")
  (setq rust-indent-method-chain t)

  (map! :map rust-mode-map
        :localleader
        :n "b" #'+rust/build-menu)
  (def-menu! +rust/build-menu
    "TODO"
    '(("cargo run"   :exec "cargo run --color always")
      ("cargo build" :exec "cargo build --color always")
      ("cargo test"  :exec "cargo test --color always"))
    :prompt "Cargo: "))


(def-package! racer
  :after rust-mode
  :config
  (add-hook! 'rust-mode-hook #'(eldoc-mode racer-mode))
  (set! :lookup 'rust-mode
    :definition #'racer-find-definition
    :documentation #'racer-describe))


(def-package! company-racer
  :when (featurep! :completion company)
  :after racer
  :config (set! :company-backend 'rust-mode '(company-racer)))


(def-package! flycheck-rust
  :when (featurep! :feature syntax-checker)
  :after rust-mode
  :hook (flycheck-mode . flycheck-rust-setup)
  :init (add-hook 'rust-mode-hook #'flycheck-mode))

