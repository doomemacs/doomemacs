;;; lang/rust/config.el -*- lexical-binding: t; -*-

;; DEPRECATED: Remove when projectile is replaced with project.el
(after! projectile
  (add-to-list 'projectile-project-root-files "Cargo.toml"))


;;
;;; Packages

(use-package! rustic
  :mode ("\\.rs\\'" . rust-mode)
  :mode ("\\.rs\\'" . rustic-mode)
  :preface
  ;; HACK: `rust-mode' and `rustic' add entries to `auto-mode-alist', but
  ;;   package load order makes which gets precedence unpredictable. By removing
  ;;   them early, we rely on the `:mode' directives above to re-insert them
  ;;   with the correct order.
  (setq auto-mode-alist (assoc-delete-all "\\.rs\\'" auto-mode-alist))

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

  ;; Leave automatic reformatting to the :editor format module.
  (setq rustic-babel-format-src-block nil
        rustic-format-trigger nil)

  (if (modulep! -lsp)
      (after! rustic-flycheck
        (add-to-list 'flycheck-checkers 'rustic-clippy))
    (setq rustic-lsp-client
          (if (modulep! :tools lsp +eglot)
              'eglot
            'lsp-mode))
    (add-hook 'rustic-mode-local-vars-hook #'rustic-setup-lsp 'append)

    ;; HACK: Add @scturtle fix for signatures on hover on LSP mode. This code
    ;;   has not been upstreamed because it depends on the exact format of the
    ;;   response of Rust Analyzer, which is not stable enough for `lsp-mode'
    ;;   maintainers (see emacs-lsp/lsp-mode#1740).
    (unless (modulep! :tools lsp +eglot)
      (defadvice! +rust--dont-cache-results-from-ra-a (fn &rest args)
        :after #'lsp-eldoc-function
        (when (derived-mode-p 'rust-mode 'rust-ts-mode)
          (setq lsp--hover-saved-bounds nil)))

      ;; extract and show short signature for rust-analyzer
      (cl-defmethod lsp-clients-extract-signature-on-hover (contents (_server-id (eql rust-analyzer)))
        (let* ((value (if lsp-use-plists (plist-get contents :value) (gethash "value" contents)))
               (groups (--partition-by (s-blank? it) (s-lines (s-trim value))))
               (mod-group (cond ((s-equals? "```rust" (car (-fifth-item groups))) (-third-item groups))
                                ((s-equals? "```rust" (car (-third-item groups))) (-first-item groups))
                                (t nil)))
               (cmt (if (null mod-group) "" (concat " // " (cadr mod-group))))
               (sig-group (cond ((s-equals? "```rust" (car (-fifth-item groups))) (-fifth-item groups))
                                ((s-equals? "```rust" (car (-third-item groups))) (-third-item groups))
                                (t (-first-item groups))))
               (sig (->> sig-group
                         (--drop-while (s-equals? "```rust" it))
                         (--take-while (not (s-equals? "```" it)))
                         (--map (s-replace-regexp "//.*" "" it))
                         (--map (s-trim it))
                         (s-join " "))))
          (lsp--render-element (concat "```rust\n" sig cmt "\n```"))))))

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
