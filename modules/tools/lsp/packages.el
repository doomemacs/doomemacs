;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (modulep! +eglot)
    (progn
      (package! eglot :pin "a8cb16b833d6e8b6e9ea6b995a82b14fee0eb398")
      (when (modulep! :completion vertico)
        (package! consult-eglot :pin "2816f8aad7d6a1e6d5e5b4a5e04c1d74b82b26b8"))
      (when (modulep! :checkers syntax -flymake)
        (package! flycheck-eglot :pin "0d7f0afc9bf08fce4a3ee225ec6540a91f8cfd76")))
  (package! lsp-mode :pin "65a414ddeb84d0282eda357cbd41ea674a42fd0b")
  (package! lsp-ui :pin "bbb1aa0192cce1ee39c2f36953cc5256d49534a4")
  (when (modulep! :completion ivy)
    (package! lsp-ivy :pin "3ee14a24bb0f3fd2aabec0773e43796690ef3a74"))
  (when (modulep! :completion helm)
    (package! helm-lsp :pin "54926afd10da52039f8858a99d426cae2aa4c07d"))
  (when (modulep! :completion vertico)
    (package! consult-lsp :pin "aef321d03907ca6926b0cf20ca85f672c4744000")))
