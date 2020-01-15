;; -*- no-byte-compile: t; -*-
;;; completion/ivy/packages.el

(package! swiper :pin "e22c8dfd28ab1874b71b68628666c22e1112495f")
(package! ivy)
(package! ivy-hydra)
(package! counsel)

(package! amx :pin "3af93ca066aa42dd1342f1ee0ab0d02360075613")
(package! counsel-projectile :pin "cadc6de7070458781a373b052adddf102bbed302")
(package! ivy-rich :pin "7bfc7262fda46c38636eac3080980024b5881a64")
(package! wgrep :pin "379afd89ebd76f63842c8589127d66096a8bb595")

(if (featurep! +prescient)
    (package! ivy-prescient :pin "7fd8c3b8028da4733434940c4aac1209281bef58")
  (when (featurep! +fuzzy)
    (package! flx :pin "17f5c9cb2af18aa6f52910ff4a5a63591261ced5")))

(when (featurep! +childframe)
  (package! ivy-posframe :pin "6d697ff00ac406b919eba8665b1bc18a2b423cda"))

(when (featurep! +icons)
  (package! all-the-icons-ivy :pin "babea626db20773de4c408acb2788e2b9c8277e3"))
