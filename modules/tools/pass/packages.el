;; -*- no-byte-compile: t; -*-
;;; tools/pass/packages.el

(package! pass :pin "1a9f6100153b07ac4f4d1d332501240e94c38f1e")
(package! password-store :pin "b5e965a838bb68c1227caa2cdd874ba496f10149")
(package! password-store-otp :pin "be3a00a981921ed1b2f78012944dc25eb5a0beca")

(when (modulep! :completion ivy)
  (package! ivy-pass :pin "5b523de1151f2109fdd6a8114d0af12eef83d3c5"))
(when (modulep! :completion helm)
  (package! helm-pass :pin "4ce46f1801f2e76e53482c65aa0619d427a3fbf9"))
