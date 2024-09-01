;; -*- no-byte-compile: t; -*-
;;; completion/helm/packages.el

(package! helm :pin "06e0cf01486a430b1f6792af78297837d3d77d97")
(package! helm-company :pin "4622b82353220ee6cc33468f710fa5b6b253b7f1")
(package! helm-c-yasnippet :pin "c5880e740da101fde7a995e94a7b16c330e57583")
(package! helm-descbinds :pin "ca03f02da4e54a1d0a2d5498b86e1639aa808d8c")
(package! helm-describe-modes :pin "11fb36af119b784539d31c6160002de1957408aa")
(package! helm-projectile :pin "e2e38825c975269a4971df25e79b2ae98929624e")
(package! helm-rg :pin "ee0a3c09da0c843715344919400ab0a0190cc9dc")
(package! swiper-helm :pin "93fb6db87bc6a5967898b5fd3286954cc72a0008")

(when (modulep! +childframe)
  (package! helm-posframe :pin "0b6bb016f0ff4980860a9d00574de311748c40b0"))
(when (modulep! +fuzzy)
  (package! helm-flx :pin "5220099e695a3586dba2d59640217fe378e66310"))
(when (modulep! +icons)
  (package! helm-icons :pin "0d113719ee72cb7b6bb7db29f7200d667bd86607"))
(when (modulep! :lang org)
  (package! helm-org :pin "c80e53315ce6b096e2d0e630702df924bf00bf6a"))
