;;; tools/debugger/autoload/debugger.el -*- lexical-binding: t; -*-

;;;###autoload
(defalias '+debugger/start #'dape)

;;;###autoload
(defun +debugger/quit ()
  "Quit the active debugger session."
  (interactive)
  (if-let* ((conn (and (featurep 'dape)
                       (dape--live-connection 'parent t))))
      (dape-quit conn)
    (user-error "No debugger session to quit")))
