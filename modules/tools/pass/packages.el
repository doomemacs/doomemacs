;; -*- no-byte-compile: t; -*-
;;; tools/pass/packages.el

(package! pass :pin "7651389c52919f5e0e41d9217b29c7166e3a45c2")
(package! password-store :pin "3ca13cd8882cae4083c1c478858adbf2e82dd037")
(package! password-store-otp :pin "be3a00a981921ed1b2f78012944dc25eb5a0beca")

(when (modulep! :completion ivy)
  (package! ivy-pass :pin "5b523de1151f2109fdd6a8114d0af12eef83d3c5"))
(when (modulep! :completion helm)
  (package! helm-pass :pin "4ce46f1801f2e76e53482c65aa0619d427a3fbf9"))
