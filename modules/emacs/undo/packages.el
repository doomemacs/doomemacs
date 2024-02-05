;; -*- no-byte-compile: t; -*-
;;; emacs/undo/packages.el

(if (modulep! +tree)
    (package! undo-tree :pin "f9e7eac16f674aa7ed8fa065401d26c0258a84f8")
  (package! undo-fu :pin "04961ba775142627c5fa4bb94c3e507afedaecd1")
  (package! undo-fu-session :pin "2b355c9d39b2688f859a762f2289f23fd16fadc4")
  (when (> emacs-major-version 27)  ; unsupported in 27
    (package! vundo :pin "10d011fb05a9db0cc2f641e5b5bebe4b5fb81b6f")))
