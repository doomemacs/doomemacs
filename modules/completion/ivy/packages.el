;; -*- no-byte-compile: t; -*-
;;; completion/ivy/packages.el

(package! swiper :pin "ec9421340c88ebe08f05680e22308ed57ed68a3d")
(package! ivy)
(package! ivy-avy)
(package! counsel)

(package! amx :pin "5b3aa1aae84f4a225cb8d26ab79a32f97693f023")
(package! counsel-projectile :pin "40d1e1d4bb70acb00fddd6f4df9778bf2c52734b")
(package! ivy-rich :pin "aff9b6bd53e0fdcf350ab83c90e64e651b47dba4")
(package! wgrep :pin "49f09ab9b706d2312cab1199e1eeb1bcd3f27f6f")

(if (modulep! +prescient)
    (package! ivy-prescient :pin "87e2d2f2ddf24f591a5f70cc90d2afb4537caa18")
  (when (modulep! +fuzzy)
    (package! flx :pin "4b1346eb9a8a76ee9c9dede69738c63ad97ac5b6")))

(when (modulep! +childframe)
  (package! ivy-posframe :pin "660c773f559ac37f29ccf626af0103817c8d5e30"))

(when (modulep! +icons)
  (package! nerd-icons-ivy-rich :pin "7714b1194186cdd8353e2d80b40ae68c75aa3cd7"))
