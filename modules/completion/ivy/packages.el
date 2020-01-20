;; -*- no-byte-compile: t; -*-
;;; completion/ivy/packages.el

(package! swiper :pin "098f8fe5ba")
(package! ivy)
(package! ivy-hydra)
(package! counsel)

(package! amx :pin "3af93ca066")
(package! counsel-projectile :pin "cadc6de707")
(package! ivy-rich :pin "7bfc7262fd")
(package! wgrep :pin "e67e737184")

(if (featurep! +prescient)
    (package! ivy-prescient :pin "7fd8c3b802")
  (when (featurep! +fuzzy)
    (package! flx :pin "17f5c9cb2a")))

(when (featurep! +childframe)
  (package! ivy-posframe :pin "6d697ff00a"))

(when (featurep! +icons)
  (package! all-the-icons-ivy :pin "babea626db"))

(when (featurep! :tools taskrunner)
  (package! ivy-taskrunner
    :recipe (:host github :repo "emacs-taskrunner/ivy-taskrunner")
    :pin "c731ee6279f65061ef70e79d3818ea1d9678e6da"))
