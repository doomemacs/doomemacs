;; -*- no-byte-compile: t; -*-
;;; completion/ivy/packages.el

(package! swiper :pin "778d8bffa33ae0bf432aa23370bb2a535d2132ce")
(package! ivy)
(package! ivy-hydra)
(package! ivy-avy)
(package! counsel)

(package! amx :pin "ccfc92c600df681df5e8b5fecec328c462ceb71e")
(package! counsel-projectile :pin "06b03c1080d3ccc3fa9b9c41b1ccbcf13f058e4b")
(package! ivy-rich :pin "c0a164ce42920df0629c87dad5ea1bddc79ceac3")
(package! wgrep :pin "f0ef9bfa44db503cdb2f83fcfbd2fa4e2382ef1f")

(if (featurep! +prescient)
    (package! ivy-prescient :pin "5d139e5b1fe03ccaddff8c250ab8e9d795071b95")
  (when (featurep! +fuzzy)
    (package! flx :pin "647cb2f92f9936c62e277d7a74ad54a80502d255")))

(when (featurep! +childframe)
  (package! ivy-posframe :pin "08a00b81747c6b623dd1adc7491a1e2a4c3a3d2a"))

(when (featurep! +icons)
  (package! all-the-icons-ivy :pin "a70cbfa1effe36efc946a823a580cec686d5e88d"))
