;;; tools/terraform/config.el -*- lexical-binding: t; -*-

(map! :after terraform-mode
      :map terraform-mode-map
      :localleader
      :desc "terraform apply" "a" (λ! (compile "terraform apply"))
      :desc "terraform init"  "i" (λ! (compile "terraform init"))
      :desc "terraform plan"  "p" (λ! (compile "terraform plan")))


(def-package! company-terraform
  :when (featurep! :completion company)
  :after terraform-mode
  :config
  (set-company-backend! 'terraform-mode 'company-terraform))
