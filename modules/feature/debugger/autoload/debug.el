;;; feature/debugger/autoload/debug.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +debugger/quit ()
  "Quit the active debugger, if any."
  (interactive)
  (ignore-errors (call-interactively #'realgud:cmd-quit))
  (doom/popup-close)
  (when (featurep 'evil)
    (evil-normal-state)))

