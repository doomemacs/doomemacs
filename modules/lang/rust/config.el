;;; module-rust.el

(use-package rust-mode
  :mode "\\.rs$"
  :init (add-hook 'rust-mode-hook 'flycheck-mode)
  :config
  (def-builder! rust-mode "cargo run" "Cargo.toml")
  (def-builder! toml-mode "cargo run" "Cargo.toml"))

(use-package flycheck-rust
  :after rust-mode)

(defvar racer-cmd (concat doom-ext-dir "/racer"))
(defvar racer-rust-src-path (concat doom-ext-dir "/rust/src/"))
(use-package racer
  :after rust-mode
  :when (f-exists? racer-cmd)
  :init (add-hook! rust-mode '(racer-mode eldoc-mode flycheck-rust-setup))
  :config
  ;; TODO Unit test keybinds
  (def-company-backend! rust-mode (racer))
  (map! :map rust-mode-map :m "gd" 'racer-find-definition))

(provide 'module-rust)
;;; module-rust.el ends here
