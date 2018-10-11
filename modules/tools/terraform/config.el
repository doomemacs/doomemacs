;;; tools/terraform/config.el -*- lexical-binding: t; -*-

(after! terraform-mode
  (map! :map terraform-mode-map
        :localleader
        :n "a" (λ! (compile "terraform apply"))
        :n "i" (λ! (compile "terraform init"))
        :n "p" (λ! (compile "terraform plan"))))
