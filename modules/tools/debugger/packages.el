;; -*- no-byte-compile: t; -*-
;;; tools/debugger/packages.el

(when (package! realgud :pin "f73c039a340579a98e6716c901fd4e80e7eaa2eb")
  (when (featurep! :lang javascript)
    (package! realgud-trepan-ni :pin "6e38cf838c7b47b5f1353d00901b939ffa36d707")))

(when (featurep! +lsp)
  (package! dap-mode :pin "612388d0b85e77972a9c28391bac6224a63408c7")
  (package! posframe :pin "efd7ea490defc53a5b78e7469a3a35d225b766cc"))
