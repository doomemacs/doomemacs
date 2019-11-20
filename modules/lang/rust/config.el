;;; lang/rust/config.el -*- lexical-binding: t; -*-

(after! projectile
  (add-to-list 'projectile-project-root-files "Cargo.toml"))


;;
;;; Packages

(use-package! rustic
  :mode ("\\.rs$" . rustic-mode)
  :preface
  (setq rustic-rls-pkg (if (featurep! +lsp) 'lsp-mode))
  :config
  (set-docsets! 'rustic-mode "Rust")

  (setq rustic-indent-method-chain t
        rustic-flycheck-setup-mode-line-p nil
        ;; use :editor format instead
        rustic-format-on-save nil
        ;; REVIEW `rust-ordinary-lt-gt-p' is terribly expensive in large rust
        ;;        buffers, so we disable it, but only for evil users, because it
        ;;        affects `forward-sexp' and its ilk. See
        ;;        https://github.com/rust-lang/rust-mode/issues/288.
        rustic-match-angle-brackets (not (featurep! :editor evil))
        ;; `rustic-setup-rls' uses `package-installed-p' to determine if
        ;; lsp-mode/elgot are available. This breaks because Doom doesn't use
        ;; package.el to begin with (and lazy loads it). This is already handled
        ;; by the :tools lsp module, so...
        rustic-lsp-setup-p nil)

  (add-hook 'rustic-mode-hook #'rainbow-delimiters-mode)

  (when (featurep! +lsp)
    (add-hook 'rustic-mode-local-vars-hook #'lsp!)))


(use-package! racer
  :unless (featurep! +lsp)
  :hook (rustic-mode . racer-mode)
  :config
  (set-lookup-handlers! 'rustic-mode
    :definition '(racer-find-definition :async t)
    :documentation '+rust-racer-lookup-documentation))


;;
;;; Tools

(use-package! cargo
  :after rustic-mode
  :config
  (map! :map rustic-mode-map
        :localleader
        (:prefix ("b" . "build")
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
          :desc "current test" "t" #'cargo-process-current-test)))
