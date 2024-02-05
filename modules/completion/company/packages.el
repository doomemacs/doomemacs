;; -*- no-byte-compile: t; -*-
;;; completion/company/packages.el

(package! company :pin "02903bd7088d65a87df0ae0f0d0a7118de147b69")
(package! company-dict :pin "cd7b8394f6014c57897f65d335d6b2bd65dab1f4")
(when (modulep! +childframe)
  (package! company-box :pin "b6f53e26adf948aca55c3ff6c22c21a6a6614253"))
