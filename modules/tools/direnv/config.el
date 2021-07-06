;;; tools/direnv/config.el -*- lexical-binding: t; -*-

(use-package! envrc
  :when (executable-find "direnv")
  :after-call doom-first-file-hook
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
    (or (executable-find "direnv")
        (ignore (doom-log "Couldn't find direnv executable"))))

  ;; HACK envrc-mode only affects the current buffer's environment, which is
  ;;      generally what we want, except when we're running babel blocks in
  ;;      org-mode, because there may be state or envvars those blocks need to
  ;;      read. In order to perpetuate the org buffer's environment into the
  ;;      execution of the babel block we need to temporarily change the global
  ;;      environment. Let's hope it runs quickly enough that its effects aren't
  ;;      felt in other buffers in the meantime!
  (defvar +direnv--old-environment nil)
  (defadvice! +direnv-persist-environment-a (orig-fn &rest args)
    :around #'org-babel-execute-src-block
    (if +direnv--old-environment
        (apply orig-fn args)
      (setq-default +direnv--old-environment
                    (cons (default-value 'process-environment)
                          (default-value 'exec-path))
                    exec-path exec-path
                    process-environment process-environment)
      (unwind-protect (apply orig-fn args)
        (setq-default process-environment (car +direnv--old-environment)
                      exec-path (cdr +direnv--old-environment)
                      +direnv--old-environment nil)))))
