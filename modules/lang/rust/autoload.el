;;; lang/rust/autoload.el

;;;###autoload
(defun +rust/install-racer ()
  "Install and compile racer server."
  (interactive)
  (let ((racer-dir (expand-file-name "racer" +rust-cache-dir)))
    (doom-fetch :github "rust-lang/rust.git" (expand-file-name "rust" +rust-cache-dir))
    (doom-fetch :github "phildawes/racer.git" racer-dir)
    (let ((default-directory racer-dir))
      (doom-sh "cargo build --release"))))
