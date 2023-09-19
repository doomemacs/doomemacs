;; -*- no-byte-compile: t; -*-
;;; emacs/undo/packages.el

(if (modulep! +tree)
    (package! undo-tree :pin "f9e7eac16f674aa7ed8fa065401d26c0258a84f8")
  (package! undo-fu :pin "0e74116fd5c7797811a91ba4eadef50d67523eb6")
  (package! undo-fu-session :pin "a6c4f73bc22401fd36e0f2fd4fe058bb28566d84")
  (when (> emacs-major-version 27)  ; unsupported in 27
    (package! vundo :pin "24271862a2f746be038306eafe20f5eff55c4566")))
