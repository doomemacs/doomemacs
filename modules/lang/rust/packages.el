;; -*- no-byte-compile: t; -*-
;;; lang/rust/packages.el

;; requires rust cargo racer

(package! racer)
(package! rust-mode)

(when (featurep! :feature syntax-checker)
  (package! flycheck-rust))

(cond ((and (depends-on! :tools lsp)
            (featurep! +lsp))
       (package! lsp-rust))
      ((when (featurep! :completion company)
         (package! company-racer))))
