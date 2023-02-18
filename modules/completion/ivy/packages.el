;; -*- no-byte-compile: t; -*-
;;; completion/ivy/packages.el

(package! swiper :pin "b8be4913a661b557e0d3275726e36871556569d3")
(package! ivy)
(package! ivy-hydra)
(package! ivy-avy)
(package! counsel)

(package! amx :pin "37f9c7ae55eb0331b27200fb745206fc58ceffc0")
(package! counsel-projectile :pin "40d1e1d4bb70acb00fddd6f4df9778bf2c52734b")
(package! ivy-rich :pin "600b8183ed0be8668dcc548cc2c8cb94b001363b")
(package! wgrep :pin "edf768732a56840db6879706b64c5773c316d619")

(if (modulep! +prescient)
    (package! ivy-prescient :pin "35cf5d36132c3e96db9a9e4d1902dcfa207e7baa")
  (when (modulep! +fuzzy)
    (package! flx :pin "7b44a5abb254bbfbeca7a29336f7f4ebd8aabbf2")))

(when (modulep! +childframe)
  (package! ivy-posframe :pin "533a8e368fcabfd534761a5c685ce713376fa594"))

(when (modulep! +icons)
  (package! all-the-icons-ivy :pin "a70cbfa1effe36efc946a823a580cec686d5e88d"))
