;;; ui/tabbar/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +tabbar-buffer-predicate (buffer)
  "TODO"
  (or (memq buffer (window-parameter nil 'tabbar-buffers))
      (eq buffer (doom-fallback-buffer))))

;;;###autoload
(defun +tabbar-window-tab-list ()
  (+tabbar-window-buffer-list))

;;;###autoload
(defun +tabbar-window-buffer-list ()
  (cl-delete-if-not #'buffer-live-p (window-parameter nil 'tabbar-buffers)))

;;;###autoload
(defun +tabbar-buffer-groups ()
  (list
   (cond ((or (string-equal "*" (substring (buffer-name) 0 1))
              (memq major-mode '(magit-process-mode
                                 magit-status-mode
                                 magit-diff-mode
                                 magit-log-mode
                                 magit-file-mode
                                 magit-blob-mode
                                 magit-blame-mode
                                 )))
          "Emacs")
         ((derived-mode-p 'eshell-mode)
          "EShell")
         ((derived-mode-p 'dired-mode)
          "Dired")
         ((centaur-tabs-get-group-name (current-buffer))))))


;;
;;; Commands

;;;###autoload
(defun +tabbar/close-tab-or-window ()
  "TODO"
  (interactive)
  (call-interactively
   (cond ((cdr (window-parameter nil 'tabbar-buffers))
          #'kill-current-buffer)
         ((fboundp '+workspace/close-window-or-workspace)
          #'+workspace/close-window-or-workspace)
         (#'delete-window))))


;;
;;; Advice

;;;###autoload
(defun +tabbar*kill-current-buffer (&rest _)
  (+tabbar|remove-buffer))

;;;###autoload
(defun +tabbar*bury-buffer (orig-fn &rest args)
  (if centaur-tabs-mode
      (let ((b (current-buffer)))
        (apply orig-fn args)
        (unless (eq b (current-buffer))
          (with-current-buffer b
            (+tabbar|remove-buffer))))
    (apply orig-fn args)))

;;;###autoload
(defun +tabbar*kill-tab-maybe (tab)
  (let ((buffer (centaur-tabs-tab-value tab)))
    (with-current-buffer buffer
      ;; `kill-current-buffer' is advised not to kill buffers visible in another
      ;; window, so it behaves better than `kill-buffer'.
      (kill-current-buffer))
    (centaur-tabs-display-update)))


;;
;;; Hooks

;;;###autoload
(defun +tabbar|add-buffer ()
  (when (and centaur-tabs-mode
             (doom-real-buffer-p (current-buffer)))
    (let* ((this-buf (current-buffer))
           (buffers (window-parameter nil 'tabbar-buffers)))
      (cl-pushnew this-buf buffers)
      (add-hook 'kill-buffer-hook #'+tabbar|remove-buffer nil t)
      (set-window-parameter nil 'tabbar-buffers buffers))))

;;;###autoload
(defun +tabbar|remove-buffer ()
  (when centaur-tabs-mode
    (set-window-parameter
     nil
     'tabbar-buffers (delete (current-buffer) (window-parameter nil 'tabbar-buffers)))))

;;;###autoload
(defun +tabbar|new-window ()
  (when centaur-tabs-mode
    (unless (window-parameter nil 'tabbar-buffers)
      (+tabbar|add-buffer))))
