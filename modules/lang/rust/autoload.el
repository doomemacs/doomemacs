;;; lang/rust/autoload.el -*- lexical-binding: t; -*-

;; TODO (defun +rust/run-cargo () (interactive))

;;;###autoload
(defun +rust-cargo-project-p ()
  "Return t if this is a cargo project."
  (locate-dominating-file buffer-file-name "Cargo.toml"))

;;;###autoload
(defun +rust-racer-lookup-documentation (identifier)
  "A `+lookup/documentation' handler for Rust + Racer."
  (let ((buf (racer--describe identifier)))
    (when buf
      (pop-to-buffer buf)
      t)))


;;
;;; Custom Cargo commands

;;;###autoload
(defun +rust/cargo-audit ()
  "Run 'cargo audit' for the current project."
  (interactive)
  (rustic-run-cargo-command "cargo audit -f"))
