;; -*- no-byte-compile: t; -*-
;;; tools/cloudformation/packages.el

(when (featurep! :editor snippets)
  (package! aws-snippets :recipe (:host github :repo "baron42bba/aws-snippets")))
