;;; tools/terraform/doctor.el -*- lexical-binding: t; -*-

(unless (executable-find "terraform")
  (warn! "Couldn't find terraform."))
