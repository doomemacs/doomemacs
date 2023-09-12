;; -*- no-byte-compile: t; -*-
;;; tools/pass/packages.el

(package! pass :pin "c721604b3bd0c7ce7870f1a9fa0aa71f352a1009")
(package! password-store :pin "28cec11f1dbe6c4273d30370af45b69c9f408386")
(package! password-store-otp :pin "be3a00a981921ed1b2f78012944dc25eb5a0beca")

(when (modulep! :completion ivy)
  (package! ivy-pass :pin "5b523de1151f2109fdd6a8114d0af12eef83d3c5"))
(when (modulep! :completion helm)
  (package! helm-pass :pin "4ce46f1801f2e76e53482c65aa0619d427a3fbf9"))
