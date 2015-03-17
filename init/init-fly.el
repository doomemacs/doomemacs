(use-package flycheck
  :defer t
  :init
  (progn
    (setq-default flycheck-indication-mode 'right-fringe
                  ;; Removed checks on idle/change for snappiness
                  flycheck-check-syntax-automatically '(save mode-enabled)
                  flycheck-disabled-checkers '(emacs-lisp-checkdoc emacs-lisp make))
    (dolist (hook '(ruby-mode-hook
                    python-mode-hook
                    shell-mode-hook
                    ))
      (add-hook hook 'flycheck-mode)))
  :config
  (progn ; flycheck settings
    (my--cleanup-buffers-add "^\\*Flycheck.*\\*$")

    (bind 'normal flycheck-error-list-mode-map
          [escape] 'kill-this-buffer
          "q"      'kill-this-buffer)

    (evil-initial-state 'flycheck-error-list-mode 'emacs)

    (evil-ex-define-cmd "er[rors]" (λ (flycheck-buffer) (flycheck-list-errors)))

    (defun my--evil-flycheck-buffer ()
      (if (and (featurep 'flycheck) flycheck-mode)
          (flycheck-buffer)))

    ;; Check buffer when normal mode is entered
    (add-hook 'evil-normal-state-entry-hook 'my--evil-flycheck-buffer)
    ;; And on ESC in normal mode.
    (defadvice evil-force-normal-state (after evil-esc-flycheck-buffer activate)
      (my--evil-flycheck-buffer))

    (push '("^\\*Flycheck.*\\*$" :regexp t :position bottom :height 0.25 :noselect t)
          popwin:special-display-config)))

(use-package flyspell :commands flyspell-mode)


(provide 'init-fly)
;;; init-fly.el ends here
