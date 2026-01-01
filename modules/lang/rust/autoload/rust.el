;;; lang/rust/autoload/rust.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +rust-cargo-project-p ()
  "Return t if this is a cargo project."
  (locate-dominating-file buffer-file-name "Cargo.toml"))


;;
;;; Custom Cargo commands

(autoload 'rustic-run-cargo-command "rustic-cargo")
;;;###autoload
(defun +rust/cargo-audit ()
  "Run 'cargo audit' for the current project."
  (interactive)
  (rustic-run-cargo-command "cargo audit"))
