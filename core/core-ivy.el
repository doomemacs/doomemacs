;;; core-ivy.el
;; see defuns/defuns-ivy.el

(use-package ivy
  :init
  (setq projectile-completion-system 'ivy
        ivy-height 15
        ivy-do-completion-in-region nil)

  :config
  (ivy-mode +1)
  (map! :map ivy-minibuffer-map
        [escape] 'keyboard-escape-quit
        "C-r" 'evil-paste-from-register
        "M-v" 'clipboard-yank
        "C-w" 'backward-kill-word
        "C-u" 'backward-kill-sentence
        "C-b" 'backward-word
        "C-f" 'forward-word)

  (require 'counsel)
  (defun counsel-ag-function (string)
  "Grep in the current directory for STRING."
  (if (< (length string) 1)
      (counsel-more-chars 1)
    (let ((default-directory counsel--git-grep-dir)
          (regex (counsel-unquote-regex-parens
                  (setq ivy--old-re
                        (ivy--regex string)))))
      (counsel--async-command
       (format counsel-ag-base-command (shell-quote-argument regex)))
      nil))))

  (add-hook! doom-popup-mode
    (when (eq major-mode 'ivy-occur-grep-mode)
      (ivy-wgrep-change-to-wgrep-mode)))

  (define-key counsel-ag-map [backtab] 'ivy-occur))

(use-package counsel-projectile :after projectile)

(provide 'core-ivy)
;;; core-ivy.el ends here
