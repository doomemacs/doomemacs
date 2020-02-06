;;; lang/rust/config.el -*- lexical-binding: t; -*-

(after! projectile
  (add-to-list 'projectile-project-root-files "Cargo.toml"))


;;
;;; Packages

(use-package! rustic
  :mode ("\\.rs$" . rustic-mode)
  :commands rustic-run-cargo-command rustic-cargo-outdated
  :config
  (set-docsets! 'rustic-mode "Rust")

  (setq rustic-indent-method-chain t
        rustic-flycheck-setup-mode-line-p nil
        ;; use :editor format instead
        rustic-format-trigger nil
        ;; REVIEW `rust-ordinary-lt-gt-p' is terribly expensive in large rust
        ;;        buffers, so we disable it, but only for evil users, because it
        ;;        affects `forward-sexp' and its ilk. See
        ;;        https://github.com/rust-lang/rust-mode/issues/288.
        rustic-match-angle-brackets (not (featurep! :editor evil))
        ;; We use the superior default client provided by `lsp-mode', not the
        ;; one rustic-mode sets up for us.
        rustic-lsp-client nil)

  (add-hook 'rustic-mode-hook #'rainbow-delimiters-mode)

  (if (featurep! +lsp)
      (add-hook 'rustic-mode-local-vars-hook #'lsp!)
    (after! rustic-flycheck
      (add-to-list 'flycheck-checkers 'rustic-clippy)))

  (map! :map rustic-mode-map
        :localleader
        (:prefix ("b" . "build")
          :desc "cargo audit"    "a" #'+rust/cargo-audit
          :desc "cargo build"    "b" #'rustic-cargo-build
          :desc "cargo bench"    "B" #'rustic-cargo-bench
          :desc "cargo check"    "c" #'rustic-cargo-check
          :desc "cargo clippy"   "C" #'rustic-cargo-clippy
          :desc "cargo doc"      "d" #'rustic-cargo-doc
          :desc "cargo fmt"      "f" #'rustic-cargo-fmt
          :desc "cargo new"      "n" #'rustic-cargo-new
          :desc "cargo outdated" "o" #'rustic-cargo-outdated
          :desc "cargo run"      "r" #'rustic-cargo-run)
        (:prefix ("t" . "cargo test")
          :desc "all"          "a" #'rustic-cargo-test
          :desc "current test" "t" #'rustic-cargo-current-test))

  ;; If lsp/elgot isn't available, it attempts to install lsp-mode via
  ;; package.el. Doom manages its own dependencies so we disable that behavior.
  (defadvice! +rust--dont-install-packages-p (&rest _)
    :override #'rustic-install-lsp-client-p
    (message "No LSP server running")))


(use-package! racer
  :unless (featurep! +lsp)
  :hook (rustic-mode . racer-mode)
  :init
  ;; HACK Fix #2132: `racer' depends on `rust-mode', which tries to modify
  ;;      `auto-mode-alist'. We make extra sure that doesn't stick, especially
  ;;      when a buffer is reverted, as it is after rustfmt is done wiht it.
  (after! rust-mode
    (setq auto-mode-alist (delete '("\\.rs\\'" . rust-mode) auto-mode-alist)))
  :config
  (set-lookup-handlers! 'rustic-mode
    :definition '(racer-find-definition :async t)
    :documentation '+rust-racer-lookup-documentation))
