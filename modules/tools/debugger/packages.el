;; -*- no-byte-compile: t; -*-
;;; tools/debugger/packages.el

(when (package! realgud :pin "de31e772092bf40cdc7de4ab3c63efe04f8e6736")
  (when (featurep! :lang javascript)
    (package! realgud-trepan-ni :pin "6e38cf838c7b47b5f1353d00901b939ffa36d707")))

(when (featurep! +lsp)
  (package! dap-mode :pin "8c59b0a4dd0372c0b4efc74d021443894a9bd470")
  (package! posframe :pin "b3028b01a96699b1dfc9b2b5f4e3ba2bc1aa8317"))
