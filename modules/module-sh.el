;;; module-sh.el --- description

(after! sh-script
  (define-repl! sh-mode narf-inf-shell)
  (add-hook! sh-mode 'flycheck-mode)

  ;; [pedantry intensifies]
  (defadvice sh-mode (after sh-mode-rename-modeline activate)
    (setq mode-name "sh"))

  (defun narf-inf-shell ()
    (let* ((dest-sh (symbol-name sh-shell))
           (sh-shell-file dest-sh))
      (sh-shell-process t)
      (with-current-buffer "*shell*"
        (rename-buffer (format "*shell [%s]*" dest-sh))))))

(provide 'module-sh)
;;; module-sh.el ends here
