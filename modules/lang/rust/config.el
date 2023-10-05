;;; lang/rust/config.el -*- lexical-binding: t; -*-

(after! projectile
  (add-to-list 'projectile-project-root-files "Cargo.toml"))


;;
;;; Packages

(use-package! rustic
  :mode ("\\.rs$" . rustic-mode)
  :preface
  ;; HACK `rustic' sets up some things too early. I'd rather disable it and let
  ;;   our respective modules standardize how they're initialized.
  (setq rustic-lsp-client nil)
  (after! rustic-lsp
    (remove-hook 'rustic-mode-hook 'rustic-setup-lsp))
  (after! rustic-flycheck
    (remove-hook 'rustic-mode-hook #'flycheck-mode)
    (remove-hook 'rustic-mode-hook #'flymake-mode-off)
    (remove-hook 'flycheck-mode-hook #'rustic-flycheck-setup))
  :init
  ;; HACK Certainly, `rustic-babel' does this, but the package (and many other
  ;;   rustic packages) must be loaded in order for them to take effect. To lazy
  ;;   load it all, we must do it early:
  (after! org-src
    (defalias 'org-babel-execute:rust #'org-babel-execute:rustic)
    (add-to-list 'org-src-lang-modes '("rust" . rustic)))
  :config
  (add-hook 'rustic-mode-hook #'rainbow-delimiters-mode)
  (set-docsets! 'rustic-mode "Rust")
  (set-popup-rule! "^\\*rustic-compilation" :vslot -1)
  (set-popup-rule! "^\\*cargo-run" :vslot -1)

  (setq rustic-indent-method-chain t)

  ;; Conflicts with (and is redundant with) :ui ligatures
  (setq rust-prettify-symbols-alist nil)

  ;; Leave automatic reformatting to the :editor format module.
  (setq rustic-babel-format-src-block nil
        rustic-format-trigger nil)

  (if (not (modulep! +lsp))
      (after! rustic-flycheck
        (add-to-list 'flycheck-checkers 'rustic-clippy))
    (setq rustic-lsp-client
          (if (modulep! :tools lsp +eglot)
              'eglot
            'lsp-mode))
    (add-hook 'rustic-mode-local-vars-hook #'rustic-setup-lsp 'append))

  (when (modulep! +tree-sitter)
    (add-hook 'rustic-mode-local-vars-hook #'tree-sitter! 'append))

  ;; HACK If lsp/eglot isn't available, it attempts to install lsp-mode via
  ;;   package.el. Doom manages its own dependencies through straight so disable
  ;;   this behavior to avoid package-not-initialized errors.
  (defadvice! +rust--dont-install-packages-a (&rest _)
    :override #'rustic-install-lsp-client-p
    (message "No LSP server running"))

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
          :desc "all"              "a" #'rustic-cargo-test
          :desc "current test"     "t" #'rustic-cargo-current-test)))
