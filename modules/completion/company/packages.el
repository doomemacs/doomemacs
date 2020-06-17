;; -*- no-byte-compile: t; -*-
;;; completion/company/packages.el

(package! company :pin "1f56bec0ba7ce336eb8661b4d34e4b024d7dd04c")
(package! company-dict :pin "cd7b8394f6014c57897f65d335d6b2bd65dab1f4")
(package! company-prescient :pin "3ab7605d997fb8337bf5ded2ad960b98ac0e1fd7")
(when (featurep! +childframe)
  (package! company-box :pin "452f083f6c11793a3723224bce42898a2bedc0e1"))
