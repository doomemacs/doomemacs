;;; module-sh.el --- description

(after! sh-script
  ;; [pedantry intensifies]
  (defadvice sh-mode (after sh-mode-rename-modeline activate)
    (setq mode-name "sh"))

  (define-repl! sh-mode narf/inf-shell)
  (add-hook! sh-mode 'flycheck-mode)

  ;; Fontify variables in strings
  (add-hook 'sh-mode-hook 'narf|sh-extra-font-lock-activate))

(provide 'module-sh)
;;; module-sh.el ends here
