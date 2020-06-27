;; -*- no-byte-compile: t; -*-
;;; tools/debugger/packages.el

(when (package! realgud :pin "24c02f07c4ae6610278da1f04a2479dc3d9b6bdf")
  (when (featurep! :lang javascript)
    (package! realgud-trepan-ni :pin "6e9cac5e8097018aadf41c88de541168036cc227")))

(when (featurep! +lsp)
  (package! dap-mode :pin "f52fa4327147bccb7c93300d5f522e2fdcff1a70")
  (package! posframe :pin "c4459028fbe6740315ff1ed6f37e8c4decd3d050"))
