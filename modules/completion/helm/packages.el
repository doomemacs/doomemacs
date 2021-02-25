;; -*- no-byte-compile: t; -*-
;;; completion/helm/packages.el

(package! helm :pin "dbdec633c2816981d7127fe00bcd9778b2c31c51")
(package! helm-rg :pin "ee0a3c09da0c843715344919400ab0a0190cc9dc")
(package! helm-c-yasnippet :pin "28699d9a9caa8b4d37fd66368d93b6c65828c235")
(package! helm-company :pin "6eb5c2d730a60e394e005b47c1db018697094dde")
(package! helm-describe-modes
  :recipe (:host github :repo "emacs-helm/helm-describe-modes")
  :pin "11fb36af119b784539d31c6160002de1957408aa")
(package! helm-projectile :pin "58123f14c392021714fc5d23b9f95c7f95ce07f1")
(package! swiper-helm :pin "93fb6db87bc6a5967898b5fd3286954cc72a0008")
(when (featurep! +fuzzy)
  (package! helm-flx :pin "6640fac5cb16bee73c95b8ed1248a4e5e113690e"))
(when (featurep! +childframe)
  (package! posframe :pin "3454a4cb9d218c38f9c5b88798dfb2f7f85ad936"))
(when (featurep! :lang org)
  (package! helm-org :pin "b7a18dfc17e8b933956d61d68c435eee03a96c24"))
(when (featurep! +icons)
  (package! helm-icons :pin "5a668ef21ba02bf1fce2db18950858c769bf5d05"))
(package! helm-descbinds :pin "b72515982396b6e336ad7beb6767e95a80fca192")
