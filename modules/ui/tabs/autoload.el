;;; ui/tabs/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +tabs-buffer-predicate (buffer)
  "TODO"
  (or (memq buffer (window-parameter nil 'tab-buffers))
      (eq buffer (doom-fallback-buffer))))


;;
;;; Commands

;;;###autoload
(defun +tabs/close-tab-or-window ()
  "TODO"
  (interactive)
  (call-interactively
   (cond ((cdr (window-parameter nil 'tab-buffers))
          #'kill-current-buffer)
         ((fboundp '+workspace/close-window-or-workspace)
          #'+workspace/close-window-or-workspace)
         (#'delete-window))))


;;
;;; Advice

;;;###autoload
(defun +tabs-kill-current-buffer-a (&rest _)
  (+tabs-remove-buffer-h))

;;;###autoload
(defun +tabs-bury-buffer-a (orig-fn &rest args)
  (if centaur-tabs-mode
      (let ((b (current-buffer)))
        (apply orig-fn args)
        (unless (eq b (current-buffer))
          (with-current-buffer b
            (+tabs-remove-buffer-h))))
    (apply orig-fn args)))

;;;###autoload
(defun +tabs-kill-tab-maybe-a (tab)
  (let ((buffer (centaur-tabs-tab-value tab)))
    (with-current-buffer buffer
      ;; `kill-current-buffer' is advised not to kill buffers visible in another
      ;; window, so it behaves better than `kill-buffer'.
      (kill-current-buffer))
    (centaur-tabs-display-update)))


;;
;;; Hooks

;;;###autoload
(defun +tabs-add-buffer-h ()
  (when (and centaur-tabs-mode
             (doom-real-buffer-p (current-buffer)))
    (let* ((this-buf (current-buffer))
           (buffers (window-parameter nil 'tab-buffers)))
      (cl-pushnew this-buf buffers)
      (add-hook 'kill-buffer-hook #'+tabs-remove-buffer-h nil t)
      (set-window-parameter nil 'tab-buffers buffers))))

;;;###autoload
(defun +tabs-remove-buffer-h ()
  (when centaur-tabs-mode
    (set-window-parameter
     nil
     'tab-buffers (delete (current-buffer)
                          (window-parameter nil 'tab-buffers)))))

;;;###autoload
(defun +tabs-new-window-h ()
  (when centaur-tabs-mode
    (unless (window-parameter nil 'tab-buffers)
      (+tabs-add-buffer-h))))
