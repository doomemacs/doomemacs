(use-package flycheck
  :functions (flycheck-buffer)
  :commands (flycheck-mode flycheck-list-errors)
  :init
  (add-to-hooks 'flycheck-mode '(ruby-mode-hook
                                 python-mode-hook
                                 php-mode-hook
                                 lua-mode-hook
                                 shell-mode-hook
                                 scss-mode-hook
                                 c++-mode-hook
                                 c-mode-hook))
  :config
  (progn ; flycheck settings
    (defun my-shorter-fly-status (result)
      (format "[%s]" (replace-regexp-in-string " FlyC:?" "" result)))
    (advice-add 'flycheck-mode-line-status-text :filter-return 'my-shorter-fly-status)

    (setq-default flycheck-indication-mode 'right-fringe
                  ;; Removed checks on idle/change for snappiness
                  flycheck-check-syntax-automatically '(save mode-enabled idle-change)
                  flycheck-disabled-checkers '(emacs-lisp-checkdoc make))

    (narf/add-throwaway-buffer "^\\*Flycheck.*\\*$")

    (bind :normal :map flycheck-error-list-mode-map
          [escape] 'kill-this-buffer
          "q"      'kill-this-buffer)

    (evil-initial-state 'flycheck-error-list-mode 'emacs)

    (defun narf/evil-flycheck-buffer ()
      (if (and (featurep 'flycheck) flycheck-mode)
          (flycheck-buffer)))

    ;; Check buffer when normal mode is entered
    (add-hook 'evil-normal-state-entry-hook 'narf/evil-flycheck-buffer)
    ;; And on ESC in normal mode.
    (advice-add 'evil-force-normal-state :after 'narf/evil-flycheck-buffer)))

(use-package flyspell :commands flyspell-mode)


(provide 'init-fly)
;;; init-fly.el ends here
