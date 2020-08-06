;; -*- no-byte-compile: t; -*-
;;; tools/debugger/packages.el

(when (package! realgud :pin "5b0ed08364b1f5e2df2f3f7d5a6e7a2fedb5189a")
  (when (featurep! :lang javascript)
    (package! realgud-trepan-ni :pin "6e38cf838c7b47b5f1353d00901b939ffa36d707")))

(when (featurep! +lsp)
  (package! dap-mode :pin "85a61630499bb836bdb378fb62ea4ddd4f61a2dd")
  (package! posframe :pin "6d604a71deb45295f7b6ff3f98e06aeece9888be"))
