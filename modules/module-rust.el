;;; module-rust.el

;; NOTE Install instructions
;; brew install rust
;; git clone https://github.com/phildawes/racer.git ~
;; cd ~/racer
;; cargo build --release
;; mv ~/racer/release/racer /usr/local/bin
;; rm -rf ~/racer

(use-package rust-mode
  :mode "\\.rs$"
  :config
  (define-builder! rust-mode "cargo run" "Cargo.toml")
  (define-builder! toml-mode "cargo run" "Cargo.toml")

  (use-package flycheck-rust
    :config (add-hook! rust-mode 'flycheck-mode))

  (use-package racer
    :if (file-exists-p "/usr/local/bin/racer")
    :config
    (setq racer-cmd "/usr/local/bin/racer"
          racer-rust-src-path "~/Dropbox/lib/rust/src/")
    (bind! :map rust-mode-map :m "gd" 'racer-find-definition)

    (add-hook! rust-mode '(racer-mode eldoc-mode flycheck-rust-setup))
    (define-company-backend! rust-mode (racer))))

(provide 'module-rust)
;;; module-rust.el ends here
