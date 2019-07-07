;;; lang/rust/config.el -*- lexical-binding: t; -*-

(after! rust-mode
  (set-docsets! 'rust-mode "Rust")
  (setq rust-indent-method-chain t)

  (when (featurep! +lsp)
    (add-hook 'rust-mode-local-vars-hook #'lsp!))

  (def-package! cargo
    :defer t
    :init
    (map! :map rust-mode-map
          :localleader
          (:prefix "b"
            :desc "cargo add"    "a" #'cargo-process-add
            :desc "cargo build"  "b" #'cargo-process-build
            :desc "cargo bench"  "B" #'cargo-process-bench
            :desc "cargo check"  "c" #'cargo-process-check
            :desc "cargo clippy" "C" #'cargo-process-clippy
            :desc "cargo doc"    "d" #'cargo-process-doc
            :desc "cargo run"    "r" #'cargo-process-run
            :desc "cargo search" "s" #'cargo-process-search
            :desc "cargo update" "u" #'cargo-process-update)
          (:prefix ("t" . "cargo test")
            :desc "all"          "a" #'cargo-process-test
            :desc "current file" "f" #'cargo-process-current-file-tests
            :desc "current test" "t" #'cargo-process-current-test))))


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
