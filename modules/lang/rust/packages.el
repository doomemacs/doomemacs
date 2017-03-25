;; -*- no-byte-compile: t; -*-
;;; lang/rust/packages.el

(package! racer)
(package! rust-mode)

(when (featurep! :feature syntax-checker)
  (package! flycheck-rust))

(when (featurep! :completion company)
  (package! company-racer))

;;
(def-bootstrap! rust
  (pcase (doom-system-os)
    ('arch
     (let (pkgs)
       (unless (executable-find "rustc") (push "rust" pkgs))
       (unless (executable-find "cargo") (push "cargo" pkgs))
       (when pkgs
         (sudo "pacman --noconfirm -S %s" (s-join " " pkgs)))))
    ('debian) ;; TODO
    ('macos
     (unless (executable-find "rustc")
       (sh "brew install rust"))))

  (dolist (bin '("rustc" "cargo"))
    (unless (executable-find bin)
      (error "Failed to install %s" bin)))

  (require! :lang rust t)
  (require 'racer)
  (and (unless (file-directory-p racer-rust-src-path)
         (fetch :github "rust-lang/rust" (expand-file-name "rust" +rust-ext-dir))
         t)
       (unless (file-executable-p racer-cmd)
         (let ((racer-dir (expand-file-name "racer" +rust-ext-dir)))
           (fetch :github "phildawes/racer" racer-dir)
           (let ((default-directory racer-dir))
             (sh "cargo build --release"))
           t))))
