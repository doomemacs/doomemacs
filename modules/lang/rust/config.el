;;; lang/rust/config.el -*- lexical-binding: t; -*-

(after! projectile
  (add-to-list 'projectile-project-root-files "Cargo.toml"))


;;
;;; Packages

(use-package! rustic
  :mode ("\\.rs$" . rustic-mode)
  :init
  (after! org-src
    (defalias 'org-babel-execute:rust #'org-babel-execute:rustic)
    (add-to-list 'org-src-lang-modes '("rust" . rustic)))
  :config
  (setq rustic-indent-method-chain t)

  (set-docsets! 'rustic-mode "Rust")
  (set-popup-rule! "^\\*rustic-compilation" :vslot -1)

  ;; Leave automatic reformatting to the :editor format module.
  (setq rustic-babel-format-src-block nil
        rustic-format-trigger nil)

  ;; HACK `rustic-flycheck' adds all these hooks in disruptive places. Instead,
  ;;      leave it to our :checkers syntax module to do all the set up properly.
  (remove-hook 'rustic-mode-hook #'flycheck-mode)
  (remove-hook 'rustic-mode-hook #'flymake-mode-off)
  (unless (featurep! +lsp)
    (after! flycheck
      (add-to-list 'flycheck-checkers 'rustic-clippy)))

  ;; HACK `rustic-lsp' sets up lsp-mode/eglot too early. We move it to
  ;;      `rustic-mode-local-vars-hook' so file/dir local variables can be used
  ;;      to reconfigure them.
  (when (featurep! +lsp)
    (remove-hook 'rustic-mode-hook #'rustic-setup-lsp)
    (add-hook 'rustic-mode-local-vars-hook #'rustic-setup-lsp)
    (setq rustic-lsp-client
          (if (featurep! :tools lsp +eglot)
              'eglot
            'lsp-mode)))

  (map! :map rustic-mode-map
        :localleader
        (:prefix ("b" . "build")
          :desc "cargo audit"      "a" #'+rust/cargo-audit
          :desc "cargo build"      "b" #'rustic-cargo-build
          :desc "cargo bench"      "B" #'rustic-cargo-bench
          :desc "cargo check"      "c" #'rustic-cargo-check
          :desc "cargo clippy"     "C" #'rustic-cargo-clippy
          :desc "cargo doc"        "d" #'rustic-cargo-build-doc
          :desc "cargo doc --open" "D" #'rustic-cargo-doc
          :desc "cargo fmt"        "f" #'rustic-cargo-fmt
          :desc "cargo new"        "n" #'rustic-cargo-new
          :desc "cargo outdated"   "o" #'rustic-cargo-outdated
          :desc "cargo run"        "r" #'rustic-cargo-run)
        (:prefix ("t" . "cargo test")
          :desc "all"          "a" #'rustic-cargo-test
          :desc "current test" "t" #'rustic-cargo-current-test))

  ;; If lsp/eglot isn't available, it attempts to install lsp-mode via
  ;; package.el. Doom manages its own dependencies through straight so disable
  ;; this behavior to avoid package-not-initialized errors.
  (defadvice! +rust--dont-install-packages-a (&rest _)
    :override #'rustic-install-lsp-client-p
    (message "No LSP server running")))


(use-package! racer
  :unless (featurep! +lsp)
  :hook (rustic-mode-local-vars . racer-mode)
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
