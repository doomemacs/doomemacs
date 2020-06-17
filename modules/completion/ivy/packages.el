;; -*- no-byte-compile: t; -*-
;;; completion/ivy/packages.el

(package! swiper :pin "a007ba637d6c6e232fb894e10a40cfc1142a24ae")
(package! ivy)
(package! ivy-hydra)
(package! counsel)

(package! amx :pin "7fb7b874291e0cdeb1f0acb18564a686ec86788d")
(package! counsel-projectile :pin "77392cbbc42e98fc137b43f1db1b111ba6e2dd75")
(package! ivy-rich :pin "10970130b41c6ef9570893cdab8dfbe720e2b1a9")
(package! wgrep :pin "f0ef9bfa44db503cdb2f83fcfbd2fa4e2382ef1f")

(if (featurep! +prescient)
    (package! ivy-prescient :pin "3ab7605d997fb8337bf5ded2ad960b98ac0e1fd7")
  (when (featurep! +fuzzy)
    (package! flx :pin "17f5c9cb2af18aa6f52910ff4a5a63591261ced5")))

(when (featurep! +childframe)
  (package! ivy-posframe :pin "44749562a9e68bd43ccaa225b31311476fab1251"))

(when (featurep! +icons)
  (package! all-the-icons-ivy :pin "a70cbfa1effe36efc946a823a580cec686d5e88d"))
