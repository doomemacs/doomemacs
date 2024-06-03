;; -*- no-byte-compile: t; -*-
;;; tools/debugger/packages.el

(when (package! realgud :pin "365063ea8ce8ec6a852cb388088d84147421c3c2")
  (when (modulep! :lang javascript)
    (package! realgud-trepan-ni :pin "0ec088ea343835e24ae73da09bea96bfb02a3130")))

(when (modulep! +lsp)
  (package! dap-mode :pin "11431a26bc4c8ca92b097dbdbcbdc9e3d7fb5583")
  (package! posframe :pin "f4e9e509ba96ceb3c2b2b054957291607fb52651"))
