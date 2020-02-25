;;; lang/haskell/+dante.el -*- lexical-binding: t; -*-
;;;###if (featurep! +dante)

(use-package! dante
  :hook (haskell-mode-local-vars . dante-mode)
  :init
  (setq dante-load-flags '(;; defaults:
                           "+c"
                           "-Wwarn=missing-home-modules"
                           "-fno-diagnostics-show-caret"
                           ;; necessary to make attrap-attrap useful:
                           "-Wall"
                           ;; necessary to make company completion useful:
                           "-fdefer-typed-holes"
                           "-fdefer-type-errors"))
  :config
  (when (featurep! :checkers syntax)
    (flycheck-add-next-checker 'haskell-dante '(warning . haskell-hlint)))

  (set-company-backend! 'dante-mode #'dante-company)

  (defadvice! +haskell--restore-modified-state-a (orig-fn &rest args)
    "Marks the buffer as falsely modified.
Dante quietly saves the current buffer (without triggering save hooks) before
invoking flycheck, unexpectedly leaving the buffer in an unmodified state. This
is annoying if we depend on save hooks to do work on the buffer (like
reformatting)."
    :around #'dante-async-load-current-buffer
    (let ((modified-p (buffer-modified-p)))
      (apply orig-fn args)
      (if modified-p (set-buffer-modified-p t))))

  (when (featurep 'evil)
    (add-hook 'dante-mode-hook #'evil-normalize-keymaps))
  (map! :map dante-mode-map
        :localleader
        "t" #'dante-type-at
        "i" #'dante-info
        "l" #'haskell-process-load-or-reload
        "e" #'dante-eval-block
        "a" #'attrap-attrap))
