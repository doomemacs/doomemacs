;; -*- no-byte-compile: t; -*-
;;; tools/debugger/packages.el

(when (package! realgud :pin "3c88611c4ed59069093187c2a039b8d05cbe53e8")
  (when (featurep! :lang javascript)
    (package! realgud-trepan-ni :pin "0ec088ea343835e24ae73da09bea96bfb02a3130")))

(when (featurep! +lsp)
  (package! dap-mode :pin "f918c0580bd17105cbe50aa701a2375abca5a6ab")
  (package! posframe :pin "c91d4d53fa479ceb604071008ce0a901770eff57"))
