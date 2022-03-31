;; -*- no-byte-compile: t; -*-
;;; tools/pass/packages.el

(package! pass :pin "5651da53137db9adcb125b4897c2fe27eeb4368d")
(package! password-store :pin "c4d8a1d815e79ddd89a85d3e36a41d29f0475771")
(package! password-store-otp :pin "be3a00a981921ed1b2f78012944dc25eb5a0beca")

(when (featurep! :completion ivy)
  (package! ivy-pass :pin "5b523de1151f2109fdd6a8114d0af12eef83d3c5"))
(when (featurep! :completion helm)
  (package! helm-pass :pin "4ce46f1801f2e76e53482c65aa0619d427a3fbf9"))
