;; -*- no-byte-compile: t; -*-
;;; completion/ivy/packages.el

(package! swiper :pin "d2891aab7b816aebf21ebd01ce33933a6ac6244f")
(package! ivy)
(package! ivy-hydra)
(package! ivy-avy)
(package! counsel)

(package! amx :pin "ccfc92c600df681df5e8b5fecec328c462ceb71e")
(package! counsel-projectile :pin "06b03c1080d3ccc3fa9b9c41b1ccbcf13f058e4b")
(package! ivy-rich :pin "f8a1f5c90d2a113b597ef5903634c089fce3365b")
(package! wgrep :pin "f0ef9bfa44db503cdb2f83fcfbd2fa4e2382ef1f")

(if (featurep! +prescient)
    (package! ivy-prescient :pin "42adc802d3ba6c747bed7ea1f6e3ffbbdfc7192d")
  (when (featurep! +fuzzy)
    (package! flx :pin "647cb2f92f9936c62e277d7a74ad54a80502d255")))

(when (featurep! +childframe)
  (package! ivy-posframe :pin "83047d440ff132d5a45acde5955f71853edeefb9"))

(when (featurep! +icons)
  (package! all-the-icons-ivy :pin "a70cbfa1effe36efc946a823a580cec686d5e88d"))
