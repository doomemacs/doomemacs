;; -*- no-byte-compile: t; -*-
;;; lang/sml/packages.el

(package! sml-mode :pin "0338e9729bce469ad401b4af1b11fd4aeaf93242")
(when (featurep! :completion company)
  (package! company-mlton
    :recipe (:host github :repo "MatthewFluet/company-mlton" :files ("*.el" "*.basis"))
    :pin "9b09d209b4767a2af24784fb5321390ed1d445bf"))
