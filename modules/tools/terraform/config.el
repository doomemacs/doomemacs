;;; tools/terraform/config.el -*- lexical-binding: t; -*-

(when (featurep! +lsp)
  (add-hook 'terraform-mode-local-vars-hook #'lsp!))


(after! terraform-mode
  (set-docsets! 'terraform-mode "Terraform")

  (map! :map terraform-mode-map
        :localleader
        :desc "terraform apply" "a" (cmd! (compile "terraform apply" t))
        :desc "terraform init"  "i" (cmd! (compile "terraform init"))
        :desc "terraform plan"  "p" (cmd! (compile "terraform plan"))))


(use-package! company-terraform
  :when (featurep! :completion company)
  :after terraform-mode
  :config
  (set-company-backend! 'terraform-mode 'company-terraform))
