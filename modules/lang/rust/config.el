;;; lang/rust/config.el -*- lexical-binding: t; -*-

(after! projectile
  (add-to-list 'projectile-project-root-files "Cargo.toml"))


;;
;;; Packages

(use-package! rustic
  :mode ("\\.rs$" . rustic-mode)
  :commands rustic-run-cargo-command rustic-cargo-outdated
  :init
  (after! org-src
    (defalias 'org-babel-execute:rust #'org-babel-execute:rustic)
    (add-to-list 'org-src-lang-modes '("rust" . rustic)))
  :config
  (set-docsets! 'rustic-mode "Rust")
  (set-popup-rule! "^\\*rustic-compilation" :vslot -1)

  (setq rustic-indent-method-chain t
        ;; use :editor format instead
        rustic-format-trigger nil)

  (if (featurep! +lsp)
      (add-hook 'rustic-mode-local-vars-hook #'lsp!)
    (setq rustic-lsp-server nil)
    (after! rustic-flycheck
      (add-to-list 'flycheck-checkers 'rustic-clippy)))

  (when (featurep! +lsp)
    (if (featurep! :tools lsp +eglot)
        (setq rustic-lsp-client 'eglot)
      (setq rustic-lsp-client 'lsp-mode)))

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

  ;; HACK Fixes #2541: RLS doesn't appear to support documentSymbol, but
  ;;      lsp-rust thinks it does, and so yields imenu population to the server.
  ;;      The result is an empty imenu list. Until RLS supports documentSymbol,
  ;;      we disable `lsp-enable-imenu' is rust+RLS buffers.
  (defadvice! +rust--disable-imenu-for-lsp-mode-a (&rest _)
    :before #'rustic-lsp-mode-setup
    (when (eq rustic-lsp-server 'rls)
      (setq-local lsp-enable-imenu nil)))

  ;; If lsp/elgot isn't available, it attempts to install lsp-mode via
  ;; package.el. Doom manages its own dependencies through straight so disable
  ;; this behavior to avoid package-not-initialized errors.
  (defadvice! +rust--dont-install-packages-a (&rest _)
    :override #'rustic-install-lsp-client-p
    (message "No LSP server running")))


(use-package! racer
  :unless (featurep! +lsp)
  :hook (rustic-mode . racer-mode)
  :init
  ;; HACK Fix #2132: `racer' depends on `rust-mode', which tries to modify
  ;;      `auto-mode-alist'. We make extra sure that doesn't stick, especially
  ;;      when a buffer is reverted, as it is after rustfmt is done with it.
  (after! rust-mode
    (setq auto-mode-alist (delete '("\\.rs\\'" . rust-mode) auto-mode-alist)))
  :config
  (set-lookup-handlers! 'rustic-mode
    :definition '(racer-find-definition :async t)
    :documentation '+rust-racer-lookup-documentation))
