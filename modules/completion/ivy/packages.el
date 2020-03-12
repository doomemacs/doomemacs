;; -*- no-byte-compile: t; -*-
;;; completion/ivy/packages.el

(package! swiper :pin "5f1d9ce045")
(package! ivy)
(package! ivy-hydra)
(package! counsel)

(package! amx :pin "e512e74e83")
(package! counsel-projectile :pin "b556ed8995")
(package! ivy-rich :pin "0f22aff4c7")
(package! wgrep :pin "5977b8e000")

(if (featurep! +prescient)
    (package! ivy-prescient :pin "7fd8c3b802")
  (when (featurep! +fuzzy)
    (package! flx :pin "17f5c9cb2a")))

(when (featurep! +childframe)
  (package! ivy-posframe :pin "6d697ff00a"))

(when (featurep! +icons)
  (package! all-the-icons-ivy :pin "a70cbfa1ef"))
