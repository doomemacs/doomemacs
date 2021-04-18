;;; tools/cloudformation/config.el -*- lexical-binding: t; -*-

(when (featurep! :checkers syntax)
  (flycheck-cfn-setup))
