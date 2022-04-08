;; -*- no-byte-compile: t; -*-
;;; completion/ivy/packages.el

(package! swiper :pin "764e0d35ba63adb893743f27a979144477d9bfb9")
(package! ivy)
(package! ivy-hydra)
(package! ivy-avy)
(package! counsel)

(package! amx :pin "37f9c7ae55eb0331b27200fb745206fc58ceffc0")
(package! counsel-projectile :pin "40d1e1d4bb70acb00fddd6f4df9778bf2c52734b")
(package! ivy-rich :pin "600b8183ed0be8668dcc548cc2c8cb94b001363b")
(package! wgrep :pin "f9687c28bbc2e84f87a479b6ce04407bb97cfb23")

(if (featurep! +prescient)
    (package! ivy-prescient :pin "c5295a9eecbd2935bb57684a4422638e03bf738c")
  (when (featurep! +fuzzy)
    (package! flx :pin "e3b3f0533e44c5250ce73d728b59a7e96c692b5d")))

(when (featurep! +childframe)
  (package! ivy-posframe :pin "533a8e368fcabfd534761a5c685ce713376fa594"))

(when (featurep! +icons)
  (package! all-the-icons-ivy :pin "a70cbfa1effe36efc946a823a580cec686d5e88d"))
