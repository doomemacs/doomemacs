;; -*- no-byte-compile: t; -*-
;;; completion/ivy/packages.el

(package! swiper :pin "6a8e5611f32cf7cc77c2c6974dc2d3a18f32d5a5")
(package! ivy)
(package! ivy-hydra)
(package! ivy-avy)
(package! counsel)

(package! amx :pin "37f9c7ae55eb0331b27200fb745206fc58ceffc0")
(package! counsel-projectile :pin "06b03c1080d3ccc3fa9b9c41b1ccbcf13f058e4b")
(package! ivy-rich :pin "600b8183ed0be8668dcc548cc2c8cb94b001363b")
(package! wgrep :pin "f9687c28bbc2e84f87a479b6ce04407bb97cfb23")

(if (featurep! +prescient)
    (package! ivy-prescient :pin "027c2137a8d9e01a1d4c7b5e5d98da017dd2d48e")
  (when (featurep! +fuzzy)
    (package! flx :pin "647cb2f92f9936c62e277d7a74ad54a80502d255")))

(when (featurep! +childframe)
  (package! ivy-posframe :pin "9c8382823392d5e64fb4879055e43ab4a029e62a"))

(when (featurep! +icons)
  (package! all-the-icons-ivy :pin "a70cbfa1effe36efc946a823a580cec686d5e88d"))
