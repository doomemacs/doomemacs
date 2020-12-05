;; -*- no-byte-compile: t; -*-
;;; lang/sml/packages.el

(package! sml-mode :pin "0338e9729bce469ad401b4af1b11fd4aeaf93242")
(when (featurep! :completion company)
  (package! company-mlton
    :recipe (:host github :repo "MatthewFluet/company-mlton" :files ("*.el" "*.basis"))
    :pin "b87e36348fff9fa060b780c2019330ac5d5665ec"))
