;; -*- no-byte-compile: t; -*-
;;; completion/helm/packages.el

(package! helm :pin "c17f1c76e4")
(package! helm-rg :pin "785a80fe5c")
(package! helm-c-yasnippet :pin "65ca732b51")
(package! helm-company :pin "6eb5c2d730")
(package! helm-describe-modes
  :recipe (:host github :repo "emacs-helm/helm-describe-modes")
  :pin "11fb36af11")
(package! helm-projectile :pin "5328b74ddd")
(package! swiper-helm :pin "93fb6db87b")
(when (featurep! +fuzzy)
  (package! helm-flx :pin "6640fac5cb"))
(when (featurep! +childframe)
  (package! posframe :pin "c250771589"))
(when (featurep! :lang org)
  (package! helm-org :pin "8457e1e462"))
