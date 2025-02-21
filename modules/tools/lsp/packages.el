;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (modulep! +eglot)
    (progn
      (package! eglot :pin "2d5d494bfc14462a12f3a14d9670b842a7e9dbe8")
      (when (modulep! :completion vertico)
        (package! consult-eglot :pin "b71499f4b93bfea4e2005564c25c5bb0f9e73199"))
      (when (modulep! :checkers syntax -flymake)
        (package! flycheck-eglot :pin "18d0c9869585e6a9ea5c40678f266cf7f5bb2d2e")))
  (package! lsp-mode :pin "b383e637dcbd11da99848d7555318ac393c7cf43")
  (package! lsp-ui :pin "00e69463b4f27fb54332a0cae5d4f0dc7e39ac7f")
  (when (modulep! :completion ivy)
    (package! lsp-ivy :pin "6b2a625f08fb096a35faebf3c3ea0c8b295bdacd"))
  (when (modulep! :completion helm)
    (package! helm-lsp :pin "e740efb2abbc0ffd43f6dbcdb4527bc55723b842"))
  (when (modulep! :completion vertico)
    (package! consult-lsp :pin "aef321d03907ca6926b0cf20ca85f672c4744000")))
