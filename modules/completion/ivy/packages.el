;; -*- no-byte-compile: t; -*-
;;; completion/ivy/packages.el

(package! swiper :pin "abb9e1e5649bcd078a2a75bcb27abb42311b4f86")
(package! ivy)
(package! ivy-hydra)
(package! ivy-avy)
(package! counsel)

(package! amx :pin "5b3aa1aae84f4a225cb8d26ab79a32f97693f023")
(package! counsel-projectile :pin "40d1e1d4bb70acb00fddd6f4df9778bf2c52734b")
(package! ivy-rich :pin "aff9b6bd53e0fdcf350ab83c90e64e651b47dba4")
(package! wgrep :pin "49f09ab9b706d2312cab1199e1eeb1bcd3f27f6f")

(if (modulep! +prescient)
    (package! ivy-prescient :pin "7dd5b53886146a507f1388e0b61990f9820f9eb1")
  (when (modulep! +fuzzy)
    (package! flx :pin "4b1346eb9a8a76ee9c9dede69738c63ad97ac5b6")))

(when (modulep! +childframe)
  (package! ivy-posframe :pin "660c773f559ac37f29ccf626af0103817c8d5e30"))

(when (modulep! +icons)
  (package! nerd-icons-ivy-rich :pin "86a896bb48fed543993f96e4b288047aa7e013c9"))
