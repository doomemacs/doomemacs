;;; lang/rust/config.el -*- lexical-binding: t; -*-

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
  :config
  (unless (file-exists-p racer-cmd)
    (warn! "Couldn't find racer binary. Code completion won't work"))
  (unless (file-directory-p racer-rust-src-path)
    (warn! "Couldn't find rust source. Code completion won't work"))

  (add-hook! 'rust-mode-hook #'(eldoc-mode racer-mode))
  (set! :lookup 'rust-mode
    :definition #'racer-find-definition
    :documentation #'racer-describe))


(def-package! company-racer
  :when (featurep! :completion company)
  :after racer
  :config (set! :company-backend 'rust-mode '(company-racer)))


(def-package! flycheck-rust
  :when (featurep! :feature syntax-checker)
  :after rust-mode
  :hook (flycheck-mode . flycheck-rust-setup)
  :init (add-hook 'rust-mode-hook #'flycheck-mode))

