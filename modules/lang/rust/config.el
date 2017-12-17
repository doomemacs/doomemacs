;;; lang/rust/config.el -*- lexical-binding: t; -*-

(defvar +rust-src-dir (concat doom-etc-dir "rust/")
  "The path to Rust source library. Required by racer.")


;;
;; Plugins
;;

(def-package! rust-mode
  :mode "\\.rs$"
  :config
  (def-menu! +rust/build-menu
    "TODO"
    '(("run"   :exec "cargo run"   :cwd t :when (+rust-cargo-project-p))
      ("build" :exec "cargo build" :cwd t :when (+rust-cargo-project-p)))
    :prompt "Cargo: "))


(def-package! racer
  :after rust-mode
  :hook (rust-mode . racer-mode)
  :config
  (add-hook 'rust-mode-hook #'eldoc-mode)

  (setq racer-cmd (or (executable-find "racer")
                      (expand-file-name "racer/target/release/racer" +rust-src-dir))
        racer-rust-src-path (or (getenv "RUST_SRC_PATH")
                                (expand-file-name "rust/src/" +rust-src-dir)))

  (unless (file-exists-p racer-cmd)
    (warn "rust-mode: racer binary can't be found; auto-completion is disabled"))

  (set! :jump 'rust-mode :definition #'racer-find-definition))


(def-package! company-racer
  :when (featurep! :completion company)
  :after racer
  :config (set! :company-backend 'rust-mode '(company-racer)))


(def-package! flycheck-rust
  :when (featurep! :feature syntax-checker)
  :after rust-mode
  :hook (flycheck-mode . flycheck-rust-setup)
  :config (add-hook 'rust-mode-hook #'flycheck-mode))

