(provide 'init-fly)

(use-package flycheck
  :defer t
  :init (add-hook 'prog-mode-hook 'flycheck-mode)
  :config
  (progn ; flycheck settings
    (setq-default flycheck-indication-mode 'right-fringe
                  ;; Removed checks on idle/change for snappiness
                  flycheck-check-syntax-automatically '(save mode-enabled)
                  flycheck-disabled-checkers '(emacs-lisp-checkdoc make))

    (my--cleanup-buffers-add "^\\*Flycheck.*\\*$")

    (bind 'normal flycheck-error-list-mode-map
          [escape] 'kill-this-buffer
          "q"      'kill-this-buffer)

    (evil-initial-state 'flycheck-error-list-mode 'emacs)

    (evil-ex-define-cmd "er[rors]" 'flycheck-list-errors)

    (defun my/evil-flycheck-buffer ()
      (if (and (featurep 'flycheck) flycheck-mode)
          (flycheck-buffer)))

    ;; Check buffer when normal mode is entered
    (add-hook 'evil-normal-state-entry-hook 'my/evil-flycheck-buffer)
    ;; And on ESC in normal mode.
    (defadvice evil-force-normal-state (after evil-esc-flycheck-buffer activate)
      (my/evil-flycheck-buffer))

    (push '("^\\*Flycheck.*\\*$" :regexp t :position bottom :height 0.25)
          popwin:special-display-config)))

(use-package flyspell :commands flyspell-mode)
