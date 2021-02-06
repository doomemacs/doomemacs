;; -*- no-byte-compile: t; -*-
;;; tools/debugger/packages.el

(when (package! realgud :pin "f73c039a340579a98e6716c901fd4e80e7eaa2eb")
  (when (featurep! :lang javascript)
    (package! realgud-trepan-ni :pin "6e38cf838c7b47b5f1353d00901b939ffa36d707")))

(when (featurep! +lsp)
  (package! dap-mode :pin "c52c1a530dab420b24640a4b4710cf1e6a0177e0")
  (package! posframe :pin "8097276022676f73fc14d5311cba94aa9b3ac444"))
