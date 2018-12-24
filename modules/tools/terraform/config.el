;;; tools/terraform/config.el -*- lexical-binding: t; -*-

(map! :after terraform-mode
      :map terraform-mode-map
      :localleader
      :n "a" (λ! (compile "terraform apply"))
      :n "i" (λ! (compile "terraform init"))
      :n "p" (λ! (compile "terraform plan")))


(def-package! company-terraform
  :when (featurep! :completion company)
  :after terraform-mode
  :config
  (set-company-backend! 'terraform-mode 'company-terraform))
