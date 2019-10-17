;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/rust/doctor.el

(assert! (or (not (featurep! +lsp))
             (featurep! :tools lsp))
         "This module requires (:tools lsp)")

(unless (executable-find "rustc")
  (warn! "Couldn't find rustc binary"))

(unless (executable-find "cargo")
  (warn! "Couldn't find cargo binary"))

(if (featurep! +lsp)
    (let ((lsp-server 'rls))
      (when (require 'rustic nil t)
        (setq lsp-server rustic-lsp-server))
      (pcase lsp-server
        (`rust-analyzer
         (unless (executable-find "ra_lsp_server")
           (warn! "Couldn't find rust analyzer (ra_lsp_server)")))
        (`rls
         (unless (executable-find "rls")
           (warn! "Couldn't find rls")))))
  (when (require 'racer nil t)
    ;; racer
    (unless (file-exists-p racer-cmd)
      (warn! "Couldn't find the racer binary at `racer-cmd'"))
    ;; rust source code (rustup component add rust-src)
    (unless (file-directory-p racer-rust-src-path)
      (warn! "Couldn't find Rust's source code at RUST_SRC_PATH or `racer-rust-src-path'"))))
