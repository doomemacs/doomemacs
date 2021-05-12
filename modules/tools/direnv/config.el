;;; tools/direnv/config.el -*- lexical-binding: t; -*-

(use-package! envrc
  :when (executable-find "direnv")
  :after-call doom-first-file-hook
  :config
  (add-to-list 'doom-debug-variables 'envrc-debug)

  (set-popup-rule! "^\\*envrc\\*" :quit t :ttl 0)

  ;; I'm avoiding `global-envrc-mode' intentionally, because it has the
  ;; potential to run too late in the mode startup process (and after, say,
  ;; server hooks that may rely on that local direnv environment).
  (add-hook! 'change-major-mode-after-body-hook
    (defun +direnv-init-h ()
      (unless (or envrc-mode
                  (minibufferp)
                  (file-remote-p default-directory))
        (condition-case _
            (envrc-mode 1)
          (quit)))))

  ;; Ensure these local variables survive major mode changes, so envrc-mode is
  ;; only "activated" once per buffer.
  (put 'envrc-mode 'permanent-local t)
  (put 'envrc--status 'permanent-local t)
  (put 'process-environment 'permanent-local t)
  (put 'exec-path 'permanent-local t)
  (put 'eshell-path-env 'permanent-local t)

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
