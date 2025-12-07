;; -*- no-byte-compile: t; -*-
;;; completion/helm/packages.el

(package! helm :pin "b12d7fad584aa4a1e98be3dd1afbb68d312bb20a")
(package! helm-company :pin "4622b82353220ee6cc33468f710fa5b6b253b7f1")
(package! helm-c-yasnippet :pin "c5880e740da101fde7a995e94a7b16c330e57583")
(package! helm-descbinds :pin "0aff44badad976ebf2666a7e9b6ddf4db53e59e5")
(package! helm-describe-modes :pin "11fb36af119b784539d31c6160002de1957408aa")
(package! helm-projectile :pin "0ffb6b5f09c1d65d721c1111ebfa6cec0ba63234")
(package! helm-rg :pin "ee0a3c09da0c843715344919400ab0a0190cc9dc")
(package! swiper-helm :pin "93fb6db87bc6a5967898b5fd3286954cc72a0008")

(when (modulep! +childframe)
  (package! helm-posframe :pin "0b6bb016f0ff4980860a9d00574de311748c40b0"))
(when (modulep! +fuzzy)
  (package! helm-flx :pin "5220099e695a3586dba2d59640217fe378e66310"))
(when (modulep! +icons)
  (package! helm-icons :pin "0d113719ee72cb7b6bb7db29f7200d667bd86607"))
(when (modulep! :lang org)
  (package! helm-org :pin "4744ca7f8b35e17bafce9cb0093deb87a232699d"))
