;;; module-rust.el

(defvar +rust-ext-dir (concat doom-etc-dir "rust/")
  "TODO")

(def-package! rust-mode
  :mode "\\.rs$"
  :init
  (add-hook 'rust-mode-hook 'flycheck-mode)
  :config
  (set! :build 'cargo-run '(rust-mode toml-mode)
         '+rust-is-cargo-project-p '+rust/cargo-run))


(def-package! racer
  :after rust-mode
  :preface
  :init
  (add-hook! rust-mode '(racer-mode eldoc-mode flycheck-rust-setup))
  :config
  (setq racer-cmd (expand-file-name "racer/target/release/racer" +rust-ext-dir)
        racer-rust-src-path (expand-file-name "rust/src/" +rust-ext-dir))

  ;; TODO Unit test keybinds
  (map! :map rust-mode-map :m "gd" 'racer-find-definition))


(def-package! company-racer
  :when (featurep! :completion company)
  :after racer
  :config (set! :company-backend 'rust-mode '(company-racer)))


(def-package! flycheck-rust
  :when (featurep! :feature syntax-checker)
  :after rust-mode)

