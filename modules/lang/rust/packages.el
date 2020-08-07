;; -*- no-byte-compile: t; -*-
;;; lang/rust/packages.el

(package! rustic :pin "0ec0688c83cdf70be5eaaeacd96b067125fe968e")
(unless (featurep! +lsp)
  (package! racer :pin "a0bdf778f01e8c4b8a92591447257422ac0b455b"))
