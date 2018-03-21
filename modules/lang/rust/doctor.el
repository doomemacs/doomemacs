;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/rust/doctor.el

(when (require 'racer nil t)
  ;; racer
  (unless (file-exists-p racer-cmd)
    (warn! "Couldn't find the racer binary at `racer-cmd'"))
  ;; rust source code (rustup component add rust-src)
  (unless (file-directory-p racer-rust-src-path)
    (warn! "Couldn't find Rust's source code at RUST_SRC_PATH or `racer-rust-src-path'.")))
