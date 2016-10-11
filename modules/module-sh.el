;;; module-sh.el

(use-package sh-script
  :mode (("\\.\\(ba\\|z\\)sh$" . sh-mode)
         ("/\\.?z\\(sh\\(/.*\\|$\\)\\|profile\\|log\\(in\\|out\\)\\|sh\\(rc\\|env\\)\\)$" . sh-mode)
         ("/\\.?bash\\(/.*\\|rc\\|_profile\\)$" . sh-mode)
         ("/\\.?xinitrc$" . sh-mode)
         ("/bspwmrc$" . sh-mode))
  :init (add-hook! sh-mode '(flycheck-mode doom|sh-extra-font-lock-activate))
  :config
  (def-company-backend! sh-mode (shell))
  (def-electric! sh-mode :words ("else" "elif" "fi" "done" "then" "do" "esac" ";;"))
  (def-repl! sh-mode doom/inf-shell)
  (setq sh-indent-after-continuation 'always)

  ;; [pedantry intensifies]
  (add-hook! sh-mode (setq mode-name "sh")))

(use-package company-shell
  :after sh-script
  :config (setq company-shell-delete-duplicates t))

(provide 'module-sh)
;;; module-sh.el ends here
