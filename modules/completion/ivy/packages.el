;; -*- no-byte-compile: t; -*-
;;; completion/ivy/packages.el

(package! swiper :pin "bb7965aa645982c9a80bd9e772538a210b645942")
(package! ivy)
(package! ivy-hydra)
(package! ivy-avy)
(package! counsel)

(package! amx :pin "37f9c7ae55eb0331b27200fb745206fc58ceffc0")
(package! counsel-projectile :pin "06b03c1080d3ccc3fa9b9c41b1ccbcf13f058e4b")
(package! ivy-rich :pin "7b9b7b20c3ead81da90232cd6707dfad3c1f1eb3")
(package! wgrep :pin "f9687c28bbc2e84f87a479b6ce04407bb97cfb23")

(if (featurep! +prescient)
    (package! ivy-prescient :pin "8573df977eaceffc6607b7242ff8c0dab02aad65")
  (when (featurep! +fuzzy)
    (package! flx :pin "647cb2f92f9936c62e277d7a74ad54a80502d255")))

(when (featurep! +childframe)
  (package! ivy-posframe :pin "3132719a9a7c04431fe65f1dced8acafe789bdf6"))

(when (featurep! +icons)
  (package! all-the-icons-ivy :pin "a70cbfa1effe36efc946a823a580cec686d5e88d"))
