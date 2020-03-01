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
    (when (require 'rustic nil t)
      (pcase rustic-lsp-server
        (`rust-analyzer
         (unless (executable-find "rust-analyzer")
           (warn! "Couldn't find rust analyzer (rust-analyzer)")))
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
