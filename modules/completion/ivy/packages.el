;; -*- no-byte-compile: t; -*-
;;; completion/ivy/packages.el

(package! swiper :pin "64f05f4735")
(package! ivy)
(package! ivy-hydra)
(package! counsel)

(package! amx :pin "e512e74e83")
(package! counsel-projectile :pin "b556ed8995")
(package! ivy-rich :pin "596874d146")
(package! wgrep :pin "5977b8e000")

(if (featurep! +prescient)
    (package! ivy-prescient :pin "a194852e80")
  (when (featurep! +fuzzy)
    (package! flx :pin "17f5c9cb2a")))

(when (featurep! +childframe)
  (package! ivy-posframe :pin "6d697ff00a"))

(when (featurep! +icons)
  (package! all-the-icons-ivy :pin "a70cbfa1ef"))
