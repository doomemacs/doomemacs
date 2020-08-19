;; -*- no-byte-compile: t; -*-
;;; tools/debugger/packages.el

(when (package! realgud :pin "332d13673074bee252ae7819b0898ee7c7895d2e")
  (when (featurep! :lang javascript)
    (package! realgud-trepan-ni :pin "6e38cf838c7b47b5f1353d00901b939ffa36d707")))

(when (featurep! +lsp)
  (package! dap-mode :pin "4b185431b2b0cdec86ecfbc679f45d21fbf9fe59")
  (package! posframe :pin "7b92a54e588889a74d36d51167e067676db7be8a"))
