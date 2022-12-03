;;; tools/terraform/config.el -*- lexical-binding: t; -*-

(defvar +terraform-runner (if (executable-find "terragrunt") "terragrunt" "terraform")
  "The default runner - terraform or terragrunt")

(when (modulep! +lsp)
  (add-hook 'terraform-mode-local-vars-hook #'lsp! 'append))

(after! terraform-mode
  (set-docsets! 'terraform-mode "Terraform")

  (setq-hook! 'terraform-mode-hook compile-command +terraform-runner)

  (map! :map terraform-mode-map
        :localleader
        :desc "apply" "a" (cmd! (compile (format "%s apply" +terraform-runner) t))
        :desc "init"  "i" (cmd! (compile (format "%s init" +terraform-runner)))
        :desc "plan"  "p" (cmd! (compile (format "%s plan" +terraform-runner)))))

(use-package! company-terraform
  :when (modulep! :completion company)
  :after terraform-mode
  :config
  (set-company-backend! 'terraform-mode 'company-terraform))
