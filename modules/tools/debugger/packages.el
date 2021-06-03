;; -*- no-byte-compile: t; -*-
;;; tools/debugger/packages.el

(when (package! realgud :pin "7a70b27614c488be274898d0141ec82feb3a8d5a")
  (when (featurep! :lang javascript)
    (package! realgud-trepan-ni :pin "0ec088ea343835e24ae73da09bea96bfb02a3130")))

(when (featurep! +lsp)
  (package! dap-mode :pin "434784654e26daa7a8512ed57907829a043592d3")
  (package! posframe :pin "739d8fd1081bdd0d20dee9e437d64df58747b871"))
