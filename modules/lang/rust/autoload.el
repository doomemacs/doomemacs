;;; lang/rust/autoload.el -*- lexical-binding: t; -*-

;; TODO (defun +rust/run-cargo () (interactive))

;;;###autoload
(defun +rust-cargo-project-p ()
  "Return t if this is a cargo project."
  (locate-dominating-file buffer-file-name "Cargo.toml"))

;;;###autoload
(defun +rust-cargo-compile (command)
  "TODO"
  (let ((default-directory (+rust-cargo-project-p)))
    (compile command)))
