;;; completion/vertico/autoload/workspaces.el -*- lexical-binding: t; -*-
;;;###if (modulep! :ui workspaces)

(defun +vertico--workspace-buffer-state ()
  (let ((preview
         ;; Only preview in current window and other window.
         ;; Preview in frames and tabs is not possible since these don't get cleaned up.
         (if (memq consult--buffer-display
                   '(switch-to-buffer switch-to-buffer-other-window))
             (let ((orig-buf (current-buffer))
                   other-win
                   cleanup-buffers)
               (lambda (action cand)
                 (when (eq action 'preview)
                   (when (and (eq consult--buffer-display #'switch-to-buffer-other-window)
                              (not other-win))
                     (switch-to-buffer-other-window orig-buf)
                     (setq other-win (selected-window)))
                   (let ((win (or other-win (selected-window))))
                     (when (window-live-p win)
                       (with-selected-window win
                         (cond
                          ((and cand (get-buffer cand))
                           (unless (+workspaces-contain-buffer-p cand)
                             (cl-pushnew cand cleanup-buffers))
                           (switch-to-buffer cand 'norecord))
                          ((buffer-live-p orig-buf)
                           (switch-to-buffer orig-buf 'norecord)
                           (mapc #'tabspaces-remove-buffer cleanup-buffers)))))))))
           #'ignore)))
    (lambda (action cand)
      (funcall preview action cand))))

(defun +vertico--workspace-generate-sources ()
  "Generate list of consult buffer sources for all workspaces"
  (let* ((active-workspace (+workspaces-current-name))
         (key-range (append (cl-loop for i from ?1 to ?9 collect i)
                            (cl-loop for i from ?a to ?z collect i)
                            (cl-loop for i from ?A to ?Z collect i)))
         (i 0))
    (mapcar (lambda (name)
              (cl-incf i)
              (let ((ws (+workspaces-get name)))
                `(:name     ,name
                  :hidden   ,(not (string= active-workspace name))
                  :narrow   ,(nth (1- i) key-range)
                  :category buffer
                  :state    +vertico--workspace-buffer-state
                  :items    ,(lambda ()
                               (consult--buffer-query
                                :sort 'visibility
                                :as #'buffer-name
                                :predicate
                                (lambda (buf)
                                  (+workspaces-contain-buffer-p buf ws)))))))
            (mapcar (lambda (ws) (alist-get 'name ws))
                    (tab-bar-tabs)))))

(autoload 'consult--multi "consult")
;;;###autoload
(defun +vertico/switch-workspace-buffer (&optional force-same-workspace)
  "Switch to another buffer in the same or a specified workspace.

Type the workspace's number (starting from 1) followed by a space to display its
buffer list. Selecting a buffer in another workspace will switch to that
workspace instead. If FORCE-SAME-WORKSPACE (the prefix arg) is non-nil, that
buffer will be opened in the current workspace instead."
  (interactive "P")
  (when-let* ((buffer
               (consult--multi (+vertico--workspace-generate-sources)
                               :require-match
                               (confirm-nonexistent-file-or-buffer)
                               :prompt (format "Switch to buffer (%s): "
                                               (+workspaces-current-name))
                               :history 'consult--buffer-history
                               :sort nil)))
    (let ((origin-workspace (plist-get (cdr buffer) :name)))
      ;; Switch to the workspace the buffer belongs to, maybe
      (if (or (equal origin-workspace (+workspaces-current-name))
              force-same-workspace)
          (funcall consult--buffer-display (car buffer))
        (+workspaces-switch origin-workspace)
        (message "Switched to %S workspace" origin-workspace)
        (if-let* ((window (get-buffer-window (car buffer))))
            (select-window window)
          (funcall consult--buffer-display (car buffer)))))))

;;;###autoload
(defun +vertico/embark-open-in-new-workspace (file)
  "Open file in a new workspace."
  (interactive "GFile:")
  (+workspaces/new)
  (find-file file))
