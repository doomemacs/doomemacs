(provide 'init-shell)

(setq shell-file-name "zsh")
(setq comint-process-echoes t)
(setq comint-prompt-regexp "^$ ")
(setq comint-input-ignoredups t)
(setq comint-completion-addsuffix t)
(setq comint-prompt-read-only t)
(setq comint-get-old-input (lambda () ""))

;; Setup auto-complete-esque path completion
(add-to-list 'ac-modes 'shell-mode)
(add-hook 'shell-mode-hook (lambda ()
    (linum-mode 0)
	(yas-minor-mode -1)
    (enable-path-completion)

	;; I want ac to silently offer completion, but leave
	;; the actual tab-work to the underlying shell (ZSH is
	;; powerful enough!)
	(local-unset-key [tab])

    (evil-define-key 'normal shell-mode-map "j" nil)
    (evil-define-key 'normal shell-mode-map "k" nil)))
