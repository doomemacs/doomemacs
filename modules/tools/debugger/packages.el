;; -*- no-byte-compile: t; -*-
;;; tools/debugger/packages.el

(when (package! realgud :pin "e03446f54c7ee0b4ed3ec7300597046cf1de2bb8")
  (when (featurep! :lang javascript)
    (package! realgud-trepan-ni :pin "6e9cac5e8097018aadf41c88de541168036cc227")))

(when (featurep! +lsp)
  (package! dap-mode :pin "04d7e967f21a0ab1e2223e528baf55fe5b663882")
  (package! posframe :pin "093b29a53cbeda6d637ccc9ef4dfc47123e79b9e"))
