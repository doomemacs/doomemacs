;; -*- no-byte-compile: t; -*-
;;; completion/ivy/packages.el

(package! swiper :pin "8c30f4cab5948aa8d942a3b2bbf5fb6a94d9441d")
(package! ivy)
(package! ivy-hydra)
(package! ivy-avy)
(package! counsel)

(package! amx :pin "5b3aa1aae84f4a225cb8d26ab79a32f97693f023")
(package! counsel-projectile :pin "40d1e1d4bb70acb00fddd6f4df9778bf2c52734b")
(package! ivy-rich :pin "aff9b6bd53e0fdcf350ab83c90e64e651b47dba4")
(package! wgrep :pin "208b9d01cfffa71037527e3a324684b3ce45ddc4")

(if (modulep! +prescient)
    (package! ivy-prescient :pin "4b875be52e75f7b81e68a16b62cfbb2f2584042c")
  (when (modulep! +fuzzy)
    (package! flx :pin "4b1346eb9a8a76ee9c9dede69738c63ad97ac5b6")))

(when (modulep! +childframe)
  (package! ivy-posframe :pin "533a8e368fcabfd534761a5c685ce713376fa594"))

(when (modulep! +icons)
  (package! nerd-icons-ivy-rich :pin "7197614b27fd562e64b11c91d41bed4443aded90"))
