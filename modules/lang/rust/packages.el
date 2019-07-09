;; -*- no-byte-compile: t; -*-
;;; lang/rust/packages.el

(when EMACS26+
  (package! rustic))
(package! rust-mode)
(when (featurep! :tools flycheck)
  (package! flycheck-rust))
(unless (featurep! +lsp)
  (package! racer))

;;
(package! cargo)
