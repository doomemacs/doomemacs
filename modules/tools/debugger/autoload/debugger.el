;;; tools/debugger/autoload/debugger.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +debugger/start ()
  "Start a debugger in the current project and buffer."
  (interactive)
  (call-interactively
   (if (and (modulep! +lsp)
            (bound-and-true-p lsp-mode)
            (require 'dap-mode nil t))
       #'dap-debug
     #'dape)))

;;;###autoload
(defun +debugger/quit ()
  "Quit the active debugger session."
  (interactive)
  (if-let* ((conn (and (modulep! +lsp)
                       (require 'dap-mode nil t)
                       (dap--cur-session))))
      (dap-disconnect conn)
    (if-let* ((conn (and (featurep 'dape)
                         (dape--live-connection 'parent t))))
        (dape-quit conn)
      (user-error "No debugger session to quit"))))
