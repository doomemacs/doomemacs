;; -*- no-byte-compile: t; -*-
;;; completion/ivy/packages.el

(package! swiper :pin "9e0803cdb5b47e4e1844e8281516b46589ef26c7")
(package! ivy)
(package! ivy-hydra)
(package! counsel)

(package! amx :pin "7fb7b874291e0cdeb1f0acb18564a686ec86788d")
(package! counsel-projectile :pin "b556ed8995f375e57496f3482aef4b0def565de8")
(package! ivy-rich :pin "3f818b201769bc13cc75aa73645217e374136aca")
(package! wgrep :pin "5977b8e00051c9003ca96e9d35133e0dea68db2c")

(if (featurep! +prescient)
    (package! ivy-prescient :pin "0f4a89bdec61395138d968a38d375e63ccfbed63")
  (when (featurep! +fuzzy)
    (package! flx :pin "17f5c9cb2af18aa6f52910ff4a5a63591261ced5")))

(when (featurep! +childframe)
  (package! ivy-posframe :pin "ae9bafe94fe6b77b6fe45766ae6172646f6a5d50"))

(when (featurep! +icons)
  (package! all-the-icons-ivy :pin "a70cbfa1effe36efc946a823a580cec686d5e88d"))
