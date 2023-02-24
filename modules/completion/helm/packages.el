;; -*- no-byte-compile: t; -*-
;;; completion/helm/packages.el

(package! helm :pin "dfd6403947c5cd9f32afcd6bc92a1756cc958c82")
(package! helm-company :pin "6eb5c2d730a60e394e005b47c1db018697094dde")
(package! helm-c-yasnippet :pin "e214eec8b2875d8a7cd09006dfb6a8e15e9e4079")
(package! helm-descbinds :pin "b72515982396b6e336ad7beb6767e95a80fca192")
(package! helm-describe-modes :pin "11fb36af119b784539d31c6160002de1957408aa")
(package! helm-projectile :pin "35a2111d00c0c0c9d8743280d3f1243bb217118a")
(package! helm-rg :pin "ee0a3c09da0c843715344919400ab0a0190cc9dc")
(package! swiper-helm :pin "93fb6db87bc6a5967898b5fd3286954cc72a0008")

(when (modulep! +childframe)
  (package! helm-posframe :pin "87461b52b6f3f378c63642a33f584d4a4ba28351"))
(when (modulep! +fuzzy)
  (package! helm-flx :pin "5220099e695a3586dba2d59640217fe378e66310"))
(when (modulep! +icons)
  (package! helm-icons :pin "8d2f5e705c8b78a390677cf242024739c932fc95"))
(when (modulep! :lang org)
  (package! helm-org :pin "d67186d3a64e610c03a5f3d583488f018fb032e4"))
