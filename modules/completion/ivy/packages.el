;; -*- no-byte-compile: t; -*-
;;; completion/ivy/packages.el

(package! swiper :pin "8dc02d5b725f78d1f80904807b46f5406f129674")
(package! ivy)
(package! ivy-hydra)
(package! ivy-avy)
(package! counsel)

(package! amx :pin "5b3aa1aae84f4a225cb8d26ab79a32f97693f023")
(package! counsel-projectile :pin "40d1e1d4bb70acb00fddd6f4df9778bf2c52734b")
(package! ivy-rich :pin "aff9b6bd53e0fdcf350ab83c90e64e651b47dba4")
(package! wgrep :pin "208b9d01cfffa71037527e3a324684b3ce45ddc4")

(if (modulep! +prescient)
    (package! ivy-prescient :pin "2b8a8b41228bddb2e11eb1c200e98a9edd04797c")
  (when (modulep! +fuzzy)
    (package! flx :pin "4b1346eb9a8a76ee9c9dede69738c63ad97ac5b6")))

(when (modulep! +childframe)
  (package! ivy-posframe :pin "533a8e368fcabfd534761a5c685ce713376fa594"))

(when (modulep! +icons)
  (package! nerd-icons-ivy-rich :pin "86a896bb48fed543993f96e4b288047aa7e013c9"))
