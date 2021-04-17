;;; tools/cloudformation/doctor.el -*- lexical-binding: t; -*-

(unless (executable-find "cfn-lint")
  (warn! "Couldn't find cfn-lint."))
