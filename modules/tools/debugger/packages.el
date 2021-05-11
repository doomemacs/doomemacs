;; -*- no-byte-compile: t; -*-
;;; tools/debugger/packages.el

(when (package! realgud :pin "34557f8d8fc6af8d3763380942cc07193335c73b")
  (when (featurep! :lang javascript)
    (package! realgud-trepan-ni :pin "0ec088ea343835e24ae73da09bea96bfb02a3130")))

(when (featurep! +lsp)
  (package! dap-mode :pin "49af1b8cbd261a5f97e1b2886418dae5e51b452d")
  (package! posframe :pin "739d8fd1081bdd0d20dee9e437d64df58747b871"))
