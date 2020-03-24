;; -*- no-byte-compile: t; -*-
;;; lang/dart/packages.el

(package! dart-mode :pin "04fcd649f1")

;; Optional module features

(when (featurep! +flutter)
  (package! flutter :pin "ec92a4df84"))

;; Features according to other user selected options

(when (featurep! :editor snippets)
  (package! dart-snippets
    :recipe (:host github
                   :repo "MYDavoodeh/dart-snippets"
                   :files ("*.el" ("snippets" "snippets/*")))
    :pin "946ad5aaa5"))
