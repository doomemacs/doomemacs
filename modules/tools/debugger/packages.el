;; -*- no-byte-compile: t; -*-
;;; tools/debugger/packages.el

(when (package! realgud :pin "f0a3676a7e09e88d94afbe830808b76de2664eb0")
  (when (featurep! :lang javascript)
    (package! realgud-trepan-ni :pin "6e9cac5e8097018aadf41c88de541168036cc227")))

(when (featurep! +lsp)
  (package! dap-mode :pin "f0ed63e183dcd2e7f9eebe4f74d2c13808318bb9")
  (package! posframe :pin "922e4d239f7a083213d856de67a9686a091b1e27"))
