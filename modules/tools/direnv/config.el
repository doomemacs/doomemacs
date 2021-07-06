;;; tools/direnv/config.el -*- lexical-binding: t; -*-

(use-package! envrc
  :when (executable-find "direnv")
  :hook (doom-first-file . envrc-global-mode)
  :config
  (add-to-list 'doom-debug-variables 'envrc-debug)

  (set-popup-rule! "^\\*envrc\\*" :quit t :ttl 0)

  ;; A globalized minor mode triggers on `after-change-major-mode-hook'
  ;; normally, which runs after a major mode's body and hooks. If those hooks do
  ;; any initialization work that's sensitive to environmental state set up by
  ;; direnv, then you're gonna have a bad time, so I move the trigger to
  ;; `change-major-mode-after-body-hook' instead. This runs before said hooks
  ;; (but not the body; fingers crossed that no major mode does important env
  ;; initialization there).
  (add-hook! 'envrc-global-mode-hook
    (defun +direnv-init-global-mode-earlier-h ()
      (let ((fn #'envrc-global-mode-enable-in-buffers))
        (if (not envrc-global-mode)
            (remove-hook 'change-major-mode-after-body-hook fn)
          (remove-hook 'after-change-major-mode-hook fn)
          (add-hook 'change-major-mode-after-body-hook fn 100)))))

  (defadvice! +direnv--fail-gracefully-a (&rest _)
    "Don't try to use direnv if the executable isn't present."
    :before-while #'envrc-mode
    (or (get 'envrc-mode 'direnv-executable)
        (put 'envrc-mode 'direnv-executable (executable-find "direnv" t))
        (ignore (doom-log "Couldn't find direnv executable"))))

  ;; Ensure babel's execution environment matches the host buffer's.
  (advice-add #'org-babel-execute-src-block :around #'envrc-propagate-environment)

  ;; Make sure any envrc changes are propagated after a `doom/reload'
  (add-hook 'doom-after-reload-hook #'envrc-reload-all))
