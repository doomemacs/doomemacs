;;; core-ivy.el
;; see defuns/defuns-ivy.el

(use-package ivy
  :init
  (setq projectile-completion-system 'ivy
        ivy-height 14
        ivy-do-completion-in-region nil
        ivy-wrap t
        ;; fontify until EOL
        ivy-format-function 'ivy-format-function-line)

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

  (after! magit (setq magit-completing-read-function 'ivy-completing-read))
  (after! smex (setq smex-completion-method 'ivy))
  (after! yasnippet (push 'doom/yas-ivy-prompt yas-prompt-functions))

  ;;
  (require 'counsel)

  (add-hook! doom-popup-mode
    (when (eq major-mode 'ivy-occur-grep-mode)
      (ivy-wgrep-change-to-wgrep-mode)))

  (advice-add 'counsel-ag-function :override 'doom*counsel-ag-function)
  (define-key counsel-ag-map [backtab] 'doom/counsel-ag-occur)

  (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)"))

(use-package counsel-projectile :after projectile)

(provide 'core-ivy)
;;; core-ivy.el ends here
