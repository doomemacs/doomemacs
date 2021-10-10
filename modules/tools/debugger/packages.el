;; -*- no-byte-compile: t; -*-
;;; tools/debugger/packages.el

(when (package! realgud :pin "7a70b27614c488be274898d0141ec82feb3a8d5a")
  (when (featurep! :lang javascript)
    (package! realgud-trepan-ni :pin "0ec088ea343835e24ae73da09bea96bfb02a3130")))

(when (featurep! +lsp)
  (package! dap-mode :pin "a225e73026f28b67f1837e80497fe3ec641373f4")
  (package! posframe :pin "f97c4aff2c2c376ca62276d5597aa108546633a9"))
