;; -*- no-byte-compile: t; -*-
;;; completion/helm/packages.el

(package! helm :pin "05d70ff116a696f5c18e5ad569573d8936ff01da")
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
  (package! posframe :pin "c25077158980d8322f67fc59d999c2f6e8a020b2"))
(when (featurep! :lang org)
  (package! helm-org :pin "8457e1e46227bf87726e05c42cec5a4b51c2ef7b"))
