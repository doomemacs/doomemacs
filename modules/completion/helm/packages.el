;; -*- no-byte-compile: t; -*-
;;; completion/helm/packages.el

(package! helm :pin "58d198282230a5ba08f8a89e21c7c3dade3b5aa7")
(package! helm-rg :pin "785a80fe5cc87e27c5ea3d00a70049028d9e2847")
(package! helm-c-yasnippet :pin "89cc8561e7e57e9d1070ee3641df019c7f49c5dd")
(package! helm-company :pin "6eb5c2d730a60e394e005b47c1db018697094dde")
(package! helm-describe-modes
  :recipe (:host github :repo "emacs-helm/helm-describe-modes")
  :pin "11fb36af119b784539d31c6160002de1957408aa")
(package! helm-projectile :pin "08ea35825cf26e3a7d4abaddf01b49b50cffd38d")
(package! swiper-helm :pin "93fb6db87bc6a5967898b5fd3286954cc72a0008")
(when (featurep! +fuzzy)
  (package! helm-flx :pin "6640fac5cb16bee73c95b8ed1248a4e5e113690e"))
(when (featurep! +childframe)
  (package! posframe :pin "c4459028fbe6740315ff1ed6f37e8c4decd3d050"))
(when (featurep! :lang org)
  (package! helm-org :pin "b7a18dfc17e8b933956d61d68c435eee03a96c24"))
(package! helm-descbinds :pin "b72515982396b6e336ad7beb6767e95a80fca192")
