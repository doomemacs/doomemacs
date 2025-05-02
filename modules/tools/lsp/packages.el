;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (modulep! +eglot)
    (progn
      (package! eglot :pin "928c216af52cfdd5a029e0d48907131c9a51934d")
      (when (modulep! :completion vertico)
        (package! consult-eglot :pin "b71499f4b93bfea4e2005564c25c5bb0f9e73199"))
      (when (modulep! :checkers syntax -flymake)
        (package! flycheck-eglot :pin "18d0c9869585e6a9ea5c40678f266cf7f5bb2d2e")))
  (package! lsp-mode :pin "54251da4ffcf1e604296352bb78ff6ce0377351b")
  (package! lsp-ui :pin "a0dde8b52b4411cbac2eb053ef1515635ea0b7ed")
  (when (modulep! :completion ivy)
    (package! lsp-ivy :pin "3ee14a24bb0f3fd2aabec0773e43796690ef3a74"))
  (when (modulep! :completion helm)
    (package! helm-lsp :pin "54926afd10da52039f8858a99d426cae2aa4c07d"))
  (when (modulep! :completion vertico)
    (package! consult-lsp :pin "aef321d03907ca6926b0cf20ca85f672c4744000")))
