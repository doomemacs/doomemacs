;; -*- no-byte-compile: t; -*-
;;; tools/cloudformation/packages.el

(package! cfn-mode)

(when (featurep! :checkers syntax)
  (package! flycheck-cfn))

(when (featurep! :editor snippets)
  (package! aws-snippets :recipe (:host github :repo "baron42bba/aws-snippets")))
