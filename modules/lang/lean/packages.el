;; -*- no-byte-compile: t; -*-
;;; lang/lean/packages.el

(when (modulep! +v3)
  (package! lean-mode :pin "99d6a34dc5b12f6e996e9217fa9f6fe4a6af037a"))

(package! nael :pin "fbfb6757365cbde89d7ae0b56727315db15d31e4")
(when (and (modulep! +lsp)
           (modulep! :tools lsp -eglot))
  (package! nael-lsp
    ;; HACK: Upstream autoloads a `keymap-set' call on `nael-mode-map' but it's
    ;;   unlikely `nael' will be loaded at that point.
    ;; REVIEW: Address this upstream!
    :recipe (:build (:not autoloads))
    :pin "fbfb6757365cbde89d7ae0b56727315db15d31e4"))
