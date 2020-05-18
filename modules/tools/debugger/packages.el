;; -*- no-byte-compile: t; -*-
;;; tools/debugger/packages.el

(when (package! realgud :pin "1238d8e72945a84bb06cd39d7ded75f37105d4d2")
  (when (featurep! :lang javascript)
    (package! realgud-trepan-ni :pin "6e9cac5e8097018aadf41c88de541168036cc227")))

(when (featurep! +lsp)
  (package! dap-mode :pin "8f69dc2e3c850571758744d271061549c19e11fc")
  (package! posframe :pin "093b29a53cbeda6d637ccc9ef4dfc47123e79b9e"))
