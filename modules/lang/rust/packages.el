;; -*- no-byte-compile: t; -*-
;;; lang/rust/packages.el

;; requires rust cargo racer

(package! racer)
(package! rust-mode)

(when (featurep! :feature syntax-checker)
  (package! flycheck-rust))

(cond ((featurep! +lsp)
       (depends-on! :tools lsp
                    (package! lsp-rust)
                    :recipe (:fetcher
                             github
                             :repo "emacs-lsp/lsp-javascript"
                             :files ("lsp-typescript.el"))))
      ((when (featurep! :completion company)
         (package! company-racer))))
