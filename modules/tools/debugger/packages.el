;; -*- no-byte-compile: t; -*-
;;; tools/debugger/packages.el

(when (package! realgud :pin "365063ea8ce8ec6a852cb388088d84147421c3c2")
  (when (modulep! :lang javascript)
    (package! realgud-trepan-ni :pin "0ec088ea343835e24ae73da09bea96bfb02a3130")))

(when (modulep! +lsp)
  (package! dap-mode :pin "ffb795761273e1bdc3d0cd1ebdd43e36b7c08921")
  (package! posframe :pin "81651536827c96bf5af5265ee7918ab70e1dd5b1"))
