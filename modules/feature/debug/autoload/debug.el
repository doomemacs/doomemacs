;;; feature/debug/autoload/debug.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +debug/quit ()
  (interactive)
  (ignore-errors (call-interactively 'realgud:cmd-quit))
  (doom/popup-close)
  (evil-normal-state))

