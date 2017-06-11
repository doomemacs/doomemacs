;;; ../test/setup.el

(setq-default debug-on-error nil)

(run-hooks 'emacs-startup-hook)
(ert-run-tests-batch-and-exit)
