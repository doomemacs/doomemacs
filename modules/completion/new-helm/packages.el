;; -*- no-byte-compile: t; -*-
;;; completion/helm/packages.el

;; Version 3.8.7 2022-08-03
(package! helm :pin "4ede199d5d1b7050486a0fdeecbbbf49fef31118")
;; Version 2021-03-30
(package! helm-c-yasnippet :pin "e214eec8b2875d8a7cd09006dfb6a8e15e9e4079")
;; Version v0.2.5 2019-08-12
(package! helm-company :pin "6eb5c2d730a60e394e005b47c1db018697094dde")
;; Version 2019-05-01
(package! helm-descbinds :pin "b72515982396b6e336ad7beb6767e95a80fca192")
;; Version 2016-02-12
;; Note: The github repo has a later date (2017), but that is only an insignificant documentation change. No change to .el file.
(package! helm-describe-modes :pin "11fb36af119b784539d31c6160002de1957408aa")
;; Version 2022-08-07
(package! helm-projectile :pin "6dcc543815984f7f40e99050b1ee3b68a088e160")
;; Version 2020-07-21
(package! helm-rg :pin "ee0a3c09da0c843715344919400ab0a0190cc9dc")
;; Version 2018-01-31
(package! swiper-helm :pin "93fb6db87bc6a5967898b5fd3286954cc72a0008")

(when (modulep! +childframe)
  ;; Version 2021-11-03
  (package! helm-posframe :pin "87461b52b6f3f378c63642a33f584d4a4ba28351"))

(when (modulep! +fuzzy)
  ;; Version 2022-04-02
  (package! helm-flx :pin "27dd9e3ce385a3ca15092150e65781de14b5b00b"))

(when (modulep! +icons)
  ;; Version 2021-03-30
  (package! helm-icons :pin "8d2f5e705c8b78a390677cf242024739c932fc95"))

(when (modulep! :lang org)
  ;; Version 2021-03-24
  (package! helm-org :pin "d67186d3a64e610c03a5f3d583488f018fb032e4"))
