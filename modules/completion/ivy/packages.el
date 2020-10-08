;; -*- no-byte-compile: t; -*-
;;; completion/ivy/packages.el

(package! swiper :pin "b65e401c22ec56a008b00f651cd9536caf593d43")
(package! ivy)
(package! ivy-hydra)
(package! counsel)

(package! amx :pin "ccfc92c600df681df5e8b5fecec328c462ceb71e")
(package! counsel-projectile :pin "77392cbbc42e98fc137b43f1db1b111ba6e2dd75")
(package! ivy-rich :pin "10970130b41c6ef9570893cdab8dfbe720e2b1a9")
(package! wgrep :pin "f0ef9bfa44db503cdb2f83fcfbd2fa4e2382ef1f")

(if (featurep! +prescient)
    (package! ivy-prescient :pin "0c5d611d9fc6431dd049a71a6eda163c37617a33")
  (when (featurep! +fuzzy)
    (package! flx :pin "05600ff855020515d1243cf919cba1a6e77e7a1c")))

(when (featurep! +childframe)
  (package! ivy-posframe :pin "1e602a7bf66a5d9c97069c48eec2a10aaef0b421"))

(when (featurep! +icons)
  (package! all-the-icons-ivy :pin "a70cbfa1effe36efc946a823a580cec686d5e88d"))
