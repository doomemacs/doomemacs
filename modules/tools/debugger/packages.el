;; -*- no-byte-compile: t; -*-
;;; tools/debugger/packages.el

(when (package! realgud :pin "aff03aeef1e40d2abb244240bab9787f4b3e6035")
  (when (modulep! :lang javascript)
    (package! realgud-trepan-ni :pin "0ec088ea343835e24ae73da09bea96bfb02a3130")))

(when (modulep! +lsp)
  (package! dap-mode :pin "2879578abf72bf734675ce70e8dc977e1f43eca5")
  (package! posframe :pin "3084cb6eb366d26b0f5a4aa9baffb297178be3b8"))
