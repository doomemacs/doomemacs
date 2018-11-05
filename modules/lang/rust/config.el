;;; lang/rust/config.el -*- lexical-binding: t; -*-

(after! rust-mode
  (set-env! "RUST_SRC_PATH")
  (set-docsets! 'rust-mode "Rust")
  (setq rust-indent-method-chain t)

  (map! :map rust-mode-map
        :localleader
        :desc "cargo" :prefix "b"
        :desc "build"  :n "b" (λ! (compile "cargo build --color always"))
        :desc "check"  :n "c" (λ! (compile "cargo check --color always"))
        :desc "run"    :n "r" (λ! (compile "cargo run --color always"))
        :desc "clippy" :n "l" (λ! (compile "cargo clippy --color always"))
        :desc "test"   :n "t" (λ! (compile "cargo test --color always"))))


(def-package! racer
  :after rust-mode
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (set-lookup-handlers! 'rust-mode
    :definition #'racer-find-definition
    :documentation #'racer-describe))


(def-package! flycheck-rust
  :when (featurep! :feature syntax-checker)
  :after rust-mode
  :config (add-hook 'rust-mode-hook #'flycheck-rust-setup))

