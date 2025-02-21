;; -*- no-byte-compile: t; -*-
;;; tools/debugger/packages.el

(when (package! realgud :pin "365063ea8ce8ec6a852cb388088d84147421c3c2")
  (when (modulep! :lang javascript)
    (package! realgud-trepan-ni :pin "0ec088ea343835e24ae73da09bea96bfb02a3130")))

(when (modulep! +lsp)
  (package! dap-mode :pin "09dc16bae18315da0553589b700e50666a4595e6")
  (package! posframe :pin "12f540c9ad5da09673b2bca1132b41f94c134e82"))
