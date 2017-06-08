;;; ../test/setup.el

(setq-default debug-on-error nil)

(run-hooks 'after-init-hook 'emacs-startup-hook 'window-setup-hook)
(ert-run-tests-batch-and-exit)
