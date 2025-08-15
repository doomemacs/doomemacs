;;; tools/terraform/config.el -*- lexical-binding: t; -*-

(defvar +terraform-runner (if (executable-find "terragrunt") "terragrunt" "terraform")
  "The default runner - terraform or terragrunt")


;;
;;; Packages

(use-package! terraform-mode
  :defer t
  :config
  (set-docsets! 'terraform-mode "Terraform")
  (setq-hook! 'terraform-mode-hook compile-command +terraform-runner)

  (when (modulep! +lsp)
    (add-hook 'terraform-mode-local-vars-hook #'lsp! 'append))

  (map! :map terraform-mode-map
        :localleader
        :desc "apply"    "a" (cmd! (compile (format "%s apply" +terraform-runner) t))
        :desc "init"     "i" (cmd! (compile (format "%s init" +terraform-runner)))
        :desc "plan"     "p" (cmd! (compile (format "%s plan" +terraform-runner)))
        :desc "validate" "v" (cmd! (compile (format "%s validate" +terraform-runner)))
        :desc "fmt"      "f" (cmd! (compile (format "%s fmt" +terraform-runner)))
        :desc "destroy"  "d" (cmd! (compile (format "%s destroy" +terraform-runner)))))


(use-package! company-terraform
  :when (modulep! :completion company)
  :after terraform-mode
  :config
  (set-company-backend! 'terraform-mode 'company-terraform))
