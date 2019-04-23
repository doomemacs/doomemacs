;;; lang/haskell/+dante.el -*- lexical-binding: t; -*-
;;;###if (featurep! +dante)

(def-package! dante
  :hook (haskell-mode-local-vars . dante-mode)
  :init
  (setq dante-load-flags '(;; defaults:
                           "+c"
                           "-Wwarn=missing-home-modules"
                           "-fno-diagnostics-show-caret"
                           ;; neccessary to make attrap-attrap useful:
                           "-Wall"
                           ;; necessary to make company completion useful:
                           "-fdefer-typed-holes"
                           "-fdefer-type-errors"))
  :config
  (when (featurep! :tools flycheck)
    (flycheck-add-next-checker 'haskell-dante '(warning . haskell-hlint)))

  (set-company-backend! 'dante-mode #'dante-company)

  (defun +haskell*restore-modified-state (orig-fn &rest args)
    "Dante quietly saves the current buffer (without triggering save hooks) before
invoking flycheck, unexpectedly leaving the buffer in an unmodified state. This
is annoying if we depend on save hooks to do work on the buffer (like
reformatting), so we restore a (false) modified state."
    (let ((modified-p (buffer-modified-p)))
      (apply orig-fn args)
      (if modified-p (set-buffer-modified-p t))))
  (advice-add #'dante-async-load-current-buffer :around #'+haskell*restore-modified-state)

  (when (featurep 'evil)
    (add-hook 'dante-mode-hook #'evil-normalize-keymaps))
  (map! :map dante-mode-map
        :localleader
        "t" #'dante-type-at
        "i" #'dante-info
        "l" #'haskell-process-load-or-reload
        "e" #'dante-eval-block
        "a" #'attrap-attrap))
