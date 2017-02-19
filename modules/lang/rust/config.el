;;; module-rust.el

(@def-package rust-mode
  :mode "\\.rs$"
  :init
  (add-hook 'rust-mode-hook 'flycheck-mode)
  :config
  (@set :build 'cargo-run '(rust-mode toml-mode)
         '+rust-is-cargo-project-p '+rust/cargo-run))


(defvar racer-cmd (concat doom-cache-dir "racer"))
(defvar racer-rust-src-path (concat doom-cache-dir "rust/src/"))
(@def-package racer
  :after rust-mode
  :when (file-exists-p racer-cmd)
  :init
  (@add-hook rust-mode '(racer-mode eldoc-mode flycheck-rust-setup))
  :config
  ;; TODO Unit test keybinds
  (@set :company-backend 'rust-mode '(company-racer))
  (@map :map rust-mode-map :m "gd" 'racer-find-definition))


(@def-package company-racer
  :after racer)


(@def-package flycheck-rust
  :after rust-mode)

