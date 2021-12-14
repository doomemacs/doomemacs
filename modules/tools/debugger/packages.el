;; -*- no-byte-compile: t; -*-
;;; tools/debugger/packages.el

(when (package! realgud :pin "978b455d7da4dc41995192bfabc32092622651dd")
  (when (featurep! :lang javascript)
    (package! realgud-trepan-ni :pin "0ec088ea343835e24ae73da09bea96bfb02a3130")))

(when (featurep! +lsp)
  (package! dap-mode :pin "76cad34de8984f57c2b1e374e9c985cc7ec8dad0")
  (package! posframe :pin "3b1dc400d286b0a4bd42e518bf3e7eedb49fd1e6"))
