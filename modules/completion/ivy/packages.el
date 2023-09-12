;; -*- no-byte-compile: t; -*-
;;; completion/ivy/packages.el

(package! swiper :pin "9d630d800e856a2c984c5a62a6f0ad313a9d2228")
(package! ivy)
(package! ivy-hydra)
(package! ivy-avy)
(package! counsel)

(package! amx :pin "5b3aa1aae84f4a225cb8d26ab79a32f97693f023")
(package! counsel-projectile :pin "40d1e1d4bb70acb00fddd6f4df9778bf2c52734b")
(package! ivy-rich :pin "aff9b6bd53e0fdcf350ab83c90e64e651b47dba4")
(package! wgrep :pin "3132abd3750b8c87cbcf6942db952acfab5edccd")

(if (modulep! +prescient)
    (package! ivy-prescient :pin "d7cc55dad453c465af9ececbab94426202b5b32b")
  (when (modulep! +fuzzy)
    (package! flx :pin "7b44a5abb254bbfbeca7a29336f7f4ebd8aabbf2")))

(when (modulep! +childframe)
  (package! ivy-posframe :pin "533a8e368fcabfd534761a5c685ce713376fa594"))

(when (modulep! +icons)
  (package! nerd-icons-ivy-rich :pin "7197614b27fd562e64b11c91d41bed4443aded90"))
