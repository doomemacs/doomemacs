;; -*- no-byte-compile: t; -*-
;;; completion/ivy/packages.el

(package! swiper :pin "1deef7672b25e2cb89dfe5cc6e8865bc6f2bcd4e")
(package! ivy)
(package! ivy-hydra)
(package! ivy-avy)
(package! counsel)

(package! amx :pin "b99149715266b5c2c48f5a0fc43716d36575da5f")
(package! counsel-projectile :pin "06b03c1080d3ccc3fa9b9c41b1ccbcf13f058e4b")
(package! ivy-rich :pin "7b9b7b20c3ead81da90232cd6707dfad3c1f1eb3")
(package! wgrep :pin "f0ef9bfa44db503cdb2f83fcfbd2fa4e2382ef1f")

(if (featurep! +prescient)
    (package! ivy-prescient :pin "9631db72b95f87a50453867587f03c5862acf873")
  (when (featurep! +fuzzy)
    (package! flx :pin "647cb2f92f9936c62e277d7a74ad54a80502d255")))

(when (featurep! +childframe)
  (package! ivy-posframe :pin "3132719a9a7c04431fe65f1dced8acafe789bdf6"))

(when (featurep! +icons)
  (package! all-the-icons-ivy :pin "a70cbfa1effe36efc946a823a580cec686d5e88d"))
