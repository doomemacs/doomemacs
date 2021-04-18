;;; tools/cloudformation/doctor.el -*- lexical-binding: t; -*-

(when (featurep! :checkers syntax)
  (unless (executable-find "cfn-lint")
    (warn! "Couldn't find cfn-lint."))

  ;; The binary is with underscore but the name is with hyphen
  (unless (executable-find "cfn_nag")
    (warn! "Couldn't find cfn-nag.")))
