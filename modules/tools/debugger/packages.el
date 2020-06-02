;; -*- no-byte-compile: t; -*-
;;; tools/debugger/packages.el

(when (package! realgud :pin "1238d8e72945a84bb06cd39d7ded75f37105d4d2")
  (when (featurep! :lang javascript)
    (package! realgud-trepan-ni :pin "6e9cac5e8097018aadf41c88de541168036cc227")))

(when (featurep! +lsp)
  (package! dap-mode :pin "7ad915794b75481f12b3f02ac8f5c9bcfddd6938")
  (package! posframe :pin "6285217711bc846e565940261829b523e298f82e"))
