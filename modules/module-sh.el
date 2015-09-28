;;; module-sh.el --- description

;; [pedantry intensifies]
(after! sh-script
  (defadvice sh-mode (after sh-mode-rename-modeline activate)
    (setq mode-name "Sh"))

  (add-hook! sh-mode 'flycheck-mode))

(provide 'module-sh)
;;; module-sh.el ends here
