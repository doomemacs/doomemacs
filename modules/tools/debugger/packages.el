;; -*- no-byte-compile: t; -*-
;;; tools/debugger/packages.el

(when (package! realgud :pin "e03446f54c7ee0b4ed3ec7300597046cf1de2bb8")
  (when (featurep! :lang javascript)
    (package! realgud-trepan-ni :pin "6e9cac5e8097018aadf41c88de541168036cc227")))

(when (featurep! +lsp)
  (package! dap-mode :pin "cc2eb2fc1b2958ef01dad8c004d2f3bc4dc38bc3")
  (package! posframe :pin "093b29a53cbeda6d637ccc9ef4dfc47123e79b9e"))
