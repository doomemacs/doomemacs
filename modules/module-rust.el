;;; module-rust.el

(use-package rust-mode
  :mode "\\.rs$"
  :config
  (define-builder! rust-mode "cargo run" "Cargo.toml")
  (define-builder! toml-mode "cargo run" "Cargo.toml")

  (use-package flycheck-rust
    :config (add-hook! rust-mode 'flycheck-mode))

  (use-package racer
    :when (file-exists-p (concat narf-ext-dir "/racer"))
    :config
    (setq racer-cmd (concat narf-ext-dir "/racer")
          racer-rust-src-path (concat narf-ext-dir "/rust/src/"))
    (map! :map rust-mode-map :m "gd" 'racer-find-definition)

    ;; TODO Unit test keybinds

    (add-hook! rust-mode '(racer-mode eldoc-mode flycheck-rust-setup))
    (define-company-backend! rust-mode (racer))))

(provide 'module-rust)
;;; module-rust.el ends here
