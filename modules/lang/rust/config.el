;;; lang/rust/config.el -*- lexical-binding: t; -*-

(after! rust-mode
  (set-docsets! 'rust-mode "Rust")
  (setq rust-indent-method-chain t)

  (when (featurep! +lsp)
    (add-hook 'rust-mode-hook #'lsp!))

  (map! :map rust-mode-map
        :localleader
        :prefix "b"
        :desc "cargo build" "b" (λ! (+rust-cargo-compile "cargo build --color always"))
        :desc "cargo check" "c" (λ! (+rust-cargo-compile "cargo check --color always"))
        :desc "cargo run"   "r" (λ! (+rust-cargo-compile "cargo run --color always"))
        :desc "cargo test"  "t" (λ! (+rust-cargo-compile "cargo test --color always"))))


(def-package! racer
  :unless (featurep! +lsp)
  :after rust-mode
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (set-lookup-handlers! 'rust-mode
    :definition '(racer-find-definition :async t)
    :documentation '+rust-racer-lookup-documentation))


(def-package! flycheck-rust
  :when (featurep! :tools flycheck)
  :after rust-mode
  :config (add-hook 'rust-mode-hook #'flycheck-rust-setup))
