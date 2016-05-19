;;; module-sh.el

(use-package sh-script
  :mode (("\\.\\(ba\\|z\\)sh$" . sh-mode)
         ("/\\.?z\\(sh\\(/.*\\|$\\)\\|profile\\|log\\(in\\|out\\)\\|sh\\(rc\\|env\\)\\)$" . sh-mode)
         ("/\\.?bash\\(/.*\\|rc\\|_profile\\)$" . sh-mode))
  :init
  (add-hook! sh-mode
    '(flycheck-mode narf|sh-extra-font-lock-activate))
  :config
  (def-company-backend! sh-mode (shell))
  (def-electric! sh-mode :words ("else" "elif" "fi" "done"))
  (def-repl! sh-mode narf/inf-shell)
  (setq sh-indent-after-continuation 'always)

  ;; [pedantry intensifies]
  (add-hook! sh-mode (setq mode-name "sh"))

  (sp-with-modes '(sh-mode)
    (sp-local-pair "case"  "" :when '(("SPC")) :post-handlers '((:add narf/sp-insert-yasnippet)) :actions '(insert))
    (sp-local-pair "if"    "" :when '(("SPC")) :post-handlers '((:add narf/sp-insert-yasnippet)) :actions '(insert))
    (sp-local-pair "for"   "" :when '(("SPC")) :post-handlers '((:add narf/sp-insert-yasnippet)) :actions '(insert))
    (sp-local-pair "elif"  "" :when '(("SPC")) :post-handlers '((:add narf/sp-insert-yasnippet)) :actions '(insert))
    (sp-local-pair "while" "" :when '(("SPC")) :post-handlers '((:add narf/sp-insert-yasnippet)) :actions '(insert))))

(use-package company-shell
  :after sh-script
  :config (setq company-shell-delete-duplicates t))

(provide 'module-sh)
;;; module-sh.el ends here
