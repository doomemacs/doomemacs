;; -*- no-byte-compile: t; -*-
;;; tools/debugger/packages.el

(when (package! realgud :pin "f73c039a340579a98e6716c901fd4e80e7eaa2eb")
  (when (featurep! :lang javascript)
    (package! realgud-trepan-ni :pin "6e38cf838c7b47b5f1353d00901b939ffa36d707")))

(when (featurep! +lsp)
  (package! dap-mode :pin "5450af5c1cc7c46b1ecd47e9ba1ec2de9f62f9d9")
  (package! posframe :pin "3454a4cb9d218c38f9c5b88798dfb2f7f85ad936"))
