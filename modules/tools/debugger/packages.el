;; -*- no-byte-compile: t; -*-
;;; tools/debugger/packages.el

(when (package! realgud :pin "aff03aeef1e40d2abb244240bab9787f4b3e6035")
  (when (modulep! :lang javascript)
    (package! realgud-trepan-ni :pin "0ec088ea343835e24ae73da09bea96bfb02a3130")))

(when (modulep! +lsp)
  (package! dap-mode :pin "5d5043f962de030cadf761613199e0251c602d1e")
  (package! posframe :pin "0d23bc5f7cfac00277d83ae7ba52c48685bcbc68"))
