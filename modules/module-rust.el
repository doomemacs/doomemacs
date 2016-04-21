;;; module-rust.el

(use-package rust-mode
  :mode "\\.rs$"
  :config
  (define-builder! rust-mode "cargo run" "Cargo.toml")
  (define-builder! toml-mode "cargo run" "Cargo.toml")

  (require 'flycheck-rust)
  (add-hook 'rust-mode-hook 'flycheck-mode)

  (use-package racer
    :preface
    (setq racer-cmd (concat narf-ext-dir "/racer")
          racer-rust-src-path (concat narf-ext-dir "/rust/src/"))
    :when (file-exists-p racer-cmd)
    :config
    (define-company-backend! rust-mode (racer))
    (map! :map rust-mode-map :m "gd" 'racer-find-definition)

    ;; TODO Unit test keybinds

    (add-hook! rust-mode '(racer-mode eldoc-mode flycheck-rust-setup))))

(provide 'module-rust)
;;; module-rust.el ends here
