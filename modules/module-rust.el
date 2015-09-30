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
  (build-for! rust-mode "cargo run" "Cargo.toml")
  (use-package flycheck-rust
    :config (add-hook! rust-mode 'flycheck-mode))

  (use-package racer
    :config
    (bind! :m "gd" 'racer-find-definition)
    (setq racer-cmd "/usr/local/bin/racer"
          racer-rust-src-path "~/Dropbox/lib/rust/src/")

    (add-company-backend! rust-mode (racer))

    (add-hook! rust-mode
      (racer-activate)
      (racer-turn-on-eldoc)
      (add-hook! flycheck-mode 'flycheck-rust-setup))))

(provide 'module-rust)
;;; module-rust.el ends here
