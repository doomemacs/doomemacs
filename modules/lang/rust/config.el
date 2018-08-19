;;; lang/rust/config.el -*- lexical-binding: t; -*-

(after! rust-mode
  (set-env! "RUST_SRC_PATH")
  (set-docset! 'rust-mode "Rust")
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
  (add-hook 'rust-mode-hook #'racer-mode)
  (set-lookup-handlers! 'rust-mode
    :definition #'racer-find-definition
    :documentation #'racer-describe))


(def-package! company-racer
  :when (featurep! :completion company)
  :after racer
  :config (set-company-backend! 'rust-mode '(company-racer)))


(def-package! flycheck-rust
  :when (featurep! :feature syntax-checker)
  :after rust-mode
  :config (add-hook 'rust-mode-hook #'flycheck-rust-setup))

