;; -*- no-byte-compile: t; -*-
;;; completion/ivy/packages.el

(package! swiper :pin "098f8fe5ba")
(package! ivy)
(package! ivy-hydra)
(package! counsel)

(package! amx :pin "3af93ca066")
(package! counsel-projectile :pin "cadc6de707")
(package! ivy-rich :pin "7bfc7262fd")
(package! wgrep :pin "379afd89eb")

(if (featurep! +prescient)
    (package! ivy-prescient :pin "7fd8c3b802")
  (when (featurep! +fuzzy)
    (package! flx :pin "17f5c9cb2a")))

(when (featurep! +childframe)
  (package! ivy-posframe :pin "6d697ff00a"))

(when (featurep! +icons)
  (package! all-the-icons-ivy :pin "babea626db"))
