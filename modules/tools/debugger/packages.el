;; -*- no-byte-compile: t; -*-
;;; tools/debugger/packages.el

(when (package! realgud :pin "2cca776d28c4d6ebef033758ef01f2af2e9b3b96")
  (when (featurep! :lang javascript)
    (package! realgud-trepan-ni :pin "6e9cac5e8097018aadf41c88de541168036cc227")))

(when (featurep! :tools lsp)
  (package! dap-mode :pin "9d08eaf77d4aeb80880be85bc0591554314d0eb7"))
