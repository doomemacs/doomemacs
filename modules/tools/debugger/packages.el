;; -*- no-byte-compile: t; -*-
;;; tools/debugger/packages.el

(when (package! realgud :pin "365063ea8ce8ec6a852cb388088d84147421c3c2")
  (when (modulep! :lang javascript)
    (package! realgud-trepan-ni :pin "0ec088ea343835e24ae73da09bea96bfb02a3130")))

(when (modulep! +lsp)
  (package! dap-mode :pin "2f0c5b28578ce65ec746e4084ba72ba5c652ea79")
  (package! posframe :pin "017deece88360c7297265680d78a0bb316470716"))
