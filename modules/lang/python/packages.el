;; -*- no-byte-compile: t; -*-
;;; lang/python/packages.el

;; requires: python jedi setuptools

(package! anaconda-mode)
(package! nose)
(package! pip-requirements)

(when (featurep! :completion company)
  (package! company-anaconda))

(when (featurep! +lpy)
  (package! lpy :recipe (:fetcher github :repo "abo-abo/lpy" :files ("*")))
  (package! lispy)
  (package! function-args)
  )

(when (featurep! +conda)
  (package! conda))
