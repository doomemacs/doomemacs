;; -*- no-byte-compile: t; -*-
;;; completion/helm/packages.el

(package! helm :pin "b6db9fb47a8900704394e63b795f4a54cb4701a8")
(package! helm-rg :pin "785a80fe5cc87e27c5ea3d00a70049028d9e2847")
(package! helm-c-yasnippet :pin "65ca732b510bfc31636708aebcfe4d2d845b59b0")
(package! helm-company :pin "6eb5c2d730a60e394e005b47c1db018697094dde")
(package! helm-describe-modes
  :recipe (:host github :repo "emacs-helm/helm-describe-modes")
  :pin "11fb36af119b784539d31c6160002de1957408aa")
(package! helm-projectile :pin "5328b74dddcee8d1913803ca8167868831a07463")
(package! swiper-helm :pin "93fb6db87bc6a5967898b5fd3286954cc72a0008")
(when (featurep! +fuzzy)
  (package! helm-flx :pin "6640fac5cb16bee73c95b8ed1248a4e5e113690e"))
(when (featurep! +childframe)
  (package! posframe :pin "093b29a53cbeda6d637ccc9ef4dfc47123e79b9e"))
(when (featurep! :lang org)
  (package! helm-org :pin "b7a18dfc17e8b933956d61d68c435eee03a96c24"))
(package! helm-descbinds :pin "b72515982396b6e336ad7beb6767e95a80fca192")
