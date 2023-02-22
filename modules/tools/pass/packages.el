;; -*- no-byte-compile: t; -*-
;;; tools/pass/packages.el

(package! pass :pin "5651da53137db9adcb125b4897c2fe27eeb4368d")
(package! password-store :pin "26d2dae04bb76a87be6960861c10432820cd5d55")
(package! password-store-otp :pin "be3a00a981921ed1b2f78012944dc25eb5a0beca")

(when (modulep! :completion ivy)
  (package! ivy-pass :pin "5b523de1151f2109fdd6a8114d0af12eef83d3c5"))
(when (modulep! :completion helm)
  (package! helm-pass :pin "4ce46f1801f2e76e53482c65aa0619d427a3fbf9"))
