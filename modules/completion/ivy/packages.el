;; -*- no-byte-compile: t; -*-
;;; completion/ivy/packages.el

(package! swiper :pin "04ca16420053a3a6d34a96f0d680dd449c2e5851")
(package! ivy)
(package! ivy-hydra)
(package! counsel)

(package! amx :pin "7fb7b874291e0cdeb1f0acb18564a686ec86788d")
(package! counsel-projectile :pin "126e825bbab872b3befd9ef88660571391ebfdc3")
(package! ivy-rich :pin "3f818b201769bc13cc75aa73645217e374136aca")
(package! wgrep :pin "f0ef9bfa44db503cdb2f83fcfbd2fa4e2382ef1f")

(if (featurep! +prescient)
    (package! ivy-prescient :pin "3ab7605d997fb8337bf5ded2ad960b98ac0e1fd7")
  (when (featurep! +fuzzy)
    (package! flx :pin "17f5c9cb2af18aa6f52910ff4a5a63591261ced5")))

(when (featurep! +childframe)
  (package! ivy-posframe :pin "ae9bafe94fe6b77b6fe45766ae6172646f6a5d50"))

(when (featurep! +icons)
  (package! all-the-icons-ivy :pin "a70cbfa1effe36efc946a823a580cec686d5e88d"))
