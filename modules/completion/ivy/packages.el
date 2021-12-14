;; -*- no-byte-compile: t; -*-
;;; completion/ivy/packages.el

(package! swiper :pin "1c6b3da377a840e898b14020133f59fca9ceea1c")
(package! ivy)
(package! ivy-hydra)
(package! ivy-avy)
(package! counsel)

(package! amx :pin "37f9c7ae55eb0331b27200fb745206fc58ceffc0")
(package! counsel-projectile :pin "e30150792a96968f55f34638cbfe63eaa30839cc")
(package! ivy-rich :pin "600b8183ed0be8668dcc548cc2c8cb94b001363b")
(package! wgrep :pin "f9687c28bbc2e84f87a479b6ce04407bb97cfb23")

(if (featurep! +prescient)
    (package! ivy-prescient :pin "292ac9fe351d469f44765d487f6b9a1c1a68ad1e")
  (when (featurep! +fuzzy)
    (package! flx :pin "e3b3f0533e44c5250ce73d728b59a7e96c692b5d")))

(when (featurep! +childframe)
  (package! ivy-posframe :pin "5d9420252ca855d6d206f1f8ef5993a6be3c618f"))

(when (featurep! +icons)
  (package! all-the-icons-ivy :pin "a70cbfa1effe36efc946a823a580cec686d5e88d"))
