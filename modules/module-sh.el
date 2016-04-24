;;; module-sh.el --- description

(associate! sh-mode :match "\\.\\(ba\\|z\\)sh$")
(associate! sh-mode :match "/\\.?z\\(sh\\(/.*\\|$\\)\\|profile\\|login\\|logout\\|shrc\\|shenv\\)$")
(associate! sh-mode :match "/\\.?bash\\(/.*\\|rc\\|_profile\\)$")
(def-electric! sh-mode :words ("else" "elif" "fi" "done"))

(after! sh-script
  (def-repl! sh-mode narf/inf-shell)
  (setq sh-indent-after-continuation 'always)

  (add-hook! sh-mode (setq mode-name "sh")) ; [pedantry intensifies]
  (add-hook! sh-mode
    '(flycheck-mode
      ;; Fontify variables in strings
      narf|sh-extra-font-lock-activate))

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
