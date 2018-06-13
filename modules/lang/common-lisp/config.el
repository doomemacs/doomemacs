;;; lang/common-lisp/config.el -*- lexical-binding: t; -*-

;; `slime'
(after! sly
  (setq inferior-lisp-program "sbcl")

  (defun +common-lisp|cleanup-sly-maybe ()
    "Kill processes and leftover buffers when killing the last sly buffer."
    (unless (cl-loop for buf in (delq (current-buffer) (buffer-list))
                     if (and (buffer-local-value 'sly-mode buf)
                             (get-buffer-window buf))
                     return t)
      (dolist (conn (sly--purge-connections))
        (sly-quit-lisp-internal conn 'sly-quit-sentinel t))
      (let (kill-buffer-hook kill-buffer-query-functions)
        (mapc #'kill-buffer
              (cl-loop for buf in (delq (current-buffer) (buffer-list))
                       if (buffer-local-value 'sly-mode buf)
                       collect buf)))))

  (defun +common-lisp|init-sly ()
    "Attempt to auto-start sly when opening a lisp buffer."
    (cond ((sly-connected-p))
          ((executable-find inferior-lisp-program)
           (let ((sly-auto-start 'always))
             (sly-auto-start)
             (add-hook 'kill-buffer-hook #'+common-lisp|cleanup-sly-maybe nil t)))
          ((message "WARNING: Couldn't find `inferior-lisp-program' (%s)"
                    inferior-lisp-program))))
  (add-hook 'sly-mode-hook #'+common-lisp|init-sly))

