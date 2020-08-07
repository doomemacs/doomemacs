;; -*- no-byte-compile: t; -*-
;;; completion/helm/packages.el

(package! helm :pin "5d224cb889aea8f090c82c5ae572b8db276bcf80")
(package! helm-rg :pin "ee0a3c09da0c843715344919400ab0a0190cc9dc")
(package! helm-c-yasnippet :pin "89cc8561e7e57e9d1070ee3641df019c7f49c5dd")
(package! helm-company :pin "6eb5c2d730a60e394e005b47c1db018697094dde")
(package! helm-describe-modes
  :recipe (:host github :repo "emacs-helm/helm-describe-modes")
  :pin "11fb36af119b784539d31c6160002de1957408aa")
(package! helm-projectile :pin "2f3a2a03d6cb9419c25b432637aa11c8d2f9f3b7")
(package! swiper-helm :pin "93fb6db87bc6a5967898b5fd3286954cc72a0008")
(when (featurep! +fuzzy)
  (package! helm-flx :pin "6640fac5cb16bee73c95b8ed1248a4e5e113690e"))
(when (featurep! +childframe)
  (package! posframe :pin "922e4d239f7a083213d856de67a9686a091b1e27"))
(when (featurep! :lang org)
  (package! helm-org :pin "b7a18dfc17e8b933956d61d68c435eee03a96c24"))
(package! helm-descbinds :pin "b72515982396b6e336ad7beb6767e95a80fca192")
