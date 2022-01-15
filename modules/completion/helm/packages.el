;; -*- no-byte-compile: t; -*-
;;; completion/helm/packages.el

(package! helm :pin "4fcb36f1b9ecb540422223479aa81579139e0b95")
(package! helm-rg :pin "ee0a3c09da0c843715344919400ab0a0190cc9dc")
(package! helm-c-yasnippet :pin "e214eec8b2875d8a7cd09006dfb6a8e15e9e4079")
(package! helm-company :pin "6eb5c2d730a60e394e005b47c1db018697094dde")
(package! helm-describe-modes
  :recipe (:host github :repo "emacs-helm/helm-describe-modes")
  :pin "11fb36af119b784539d31c6160002de1957408aa")
(package! helm-projectile :pin "58123f14c392021714fc5d23b9f95c7f95ce07f1")
(package! swiper-helm :pin "93fb6db87bc6a5967898b5fd3286954cc72a0008")
(when (featurep! +fuzzy)
  (package! helm-flx :pin "6640fac5cb16bee73c95b8ed1248a4e5e113690e"))
(when (featurep! +childframe)
  (package! posframe :pin "f97c4aff2c2c376ca62276d5597aa108546633a9"))
(when (featurep! :lang org)
  (package! helm-org :pin "d67186d3a64e610c03a5f3d583488f018fb032e4"))
(when (featurep! +icons)
  (package! helm-icons :pin "8d2f5e705c8b78a390677cf242024739c932fc95"))
(package! helm-descbinds :pin "b72515982396b6e336ad7beb6767e95a80fca192")
