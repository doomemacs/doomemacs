;;; module-sh.el --- description

(associate! sh-mode :match "\\.\\(ba\\|z\\)sh$")
(associate! sh-mode :match "/\\.?z\\(sh\\(/.*\\|$\\)\\|profile\\|login\\|logout\\|shrc\\|shenv\\)$")
(associate! sh-mode :match "/\\.?bash\\(/.*\\|rc\\|_profile\\)$")
(after! sh-script
  (define-repl! sh-mode narf/inf-shell)

  (setq sh-indent-after-continuation 'always)

  (add-hook 'sh-mode-hook 'flycheck-mode)
  (add-hook 'sh-mode-hook 'narf|sh-extra-font-lock-activate) ; Fontify variables in strings
  (add-hook! sh-mode (setq mode-name "sh")) ; [pedantry intensifies]
  (add-hook! sh-mode
    (electric-indent-local-mode +1)
    (setq narf-electric-indent-words '("else" "elif" "fi" "done")))

  (require 'company-shell)
  (setq company-shell-delete-duplicates t)

  (sp-with-modes '(sh-mode)
    (sp-local-pair "case"  "" :when '(("SPC")) :post-handlers '((:add narf/sp-insert-yasnippet)) :actions '(insert))
    (sp-local-pair "if"    "" :when '(("SPC")) :post-handlers '((:add narf/sp-insert-yasnippet)) :actions '(insert))
    (sp-local-pair "for"   "" :when '(("SPC")) :post-handlers '((:add narf/sp-insert-yasnippet)) :actions '(insert))
    (sp-local-pair "elif"  "" :when '(("SPC")) :post-handlers '((:add narf/sp-insert-yasnippet)) :actions '(insert))
    (sp-local-pair "while" "" :when '(("SPC")) :post-handlers '((:add narf/sp-insert-yasnippet)) :actions '(insert))))

(provide 'module-sh)
;;; module-sh.el ends here
