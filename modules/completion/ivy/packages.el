;; -*- no-byte-compile: t; -*-
;;; completion/ivy/packages.el

(package! swiper :pin "c6b60d34ac37bf4d91a25f16d22e528f85e06938")
(package! ivy)
(package! ivy-hydra)
(package! counsel)

(package! amx :pin "ccfc92c600df681df5e8b5fecec328c462ceb71e")
(package! counsel-projectile :pin "77392cbbc42e98fc137b43f1db1b111ba6e2dd75")
(package! ivy-rich :pin "10970130b41c6ef9570893cdab8dfbe720e2b1a9")
(package! wgrep :pin "f0ef9bfa44db503cdb2f83fcfbd2fa4e2382ef1f")

(if (featurep! +prescient)
    (package! ivy-prescient :pin "b11d79b10df12c58edc3487371c2c47dfb9b50e6")
  (when (featurep! +fuzzy)
    (package! flx :pin "17f5c9cb2af18aa6f52910ff4a5a63591261ced5")))

(when (featurep! +childframe)
  (package! ivy-posframe :pin "44749562a9e68bd43ccaa225b31311476fab1251"))

(when (featurep! +icons)
  (package! all-the-icons-ivy :pin "a70cbfa1effe36efc946a823a580cec686d5e88d"))
