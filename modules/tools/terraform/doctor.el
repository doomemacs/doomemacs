;;; tools/terraform/doctor.el -*- lexical-binding: t; -*-

(unless (executable-find "terraform")
  (warn! "Couldn't find terraform."))

(when (featurep! +lsp)
  (unless (executable-find "terraform-ls"))
  (warn! "Couldn't find terraform-ls."))
