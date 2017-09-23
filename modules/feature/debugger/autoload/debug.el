;;; feature/debugger/autoload/debug.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +debugger/quit ()
  (interactive)
  (ignore-errors (call-interactively 'realgud:cmd-quit))
  (doom/popup-close)
  (evil-normal-state))

