;;; completion/vertico/autoload/workspaces.el -*- lexical-binding: t; -*-
;;;###if (featurep! :ui workspaces)

(defun +vertico--workspace-buffer-state ()
  (let ((preview
         (let ((orig-buf (current-buffer))
               cleanup-buffers)
           (lambda (cand restore)
             (when (and (not restore)
                        ;; Only preview in current window and other window.
                        ;; Preview in frames and tabs is not possible since
                        ;; these don't get cleaned up.
                        (or (eq consult--buffer-display #'switch-to-buffer)
                            (eq consult--buffer-display #'switch-to-buffer-other-window)))
               (cond
                ((and cand (get-buffer cand))
                 (unless (+workspace-contains-buffer-p cand)
                   (cl-pushnew cand cleanup-buffers))
                 (consult--buffer-action cand 'norecord))
                ((buffer-live-p orig-buf)
                 (consult--buffer-action orig-buf 'norecord)
                 (mapc #'persp-remove-buffer cleanup-buffers))))))))
    (lambda (cand restore)
      (funcall preview cand restore))))

(defun +vertico--workspace-generate-sources ()
  "Generate list of consult buffer sources for all workspaces"
  (let* ((active-workspace (+workspace-current-name))
         (workspaces (+workspace-list-names))
         (key-range (append (cl-loop for i from ?1 to ?9 collect i)
                            (cl-loop for i from ?a to ?z collect i)
                            (cl-loop for i from ?A to ?Z collect i)))
         (last-i (length workspaces))
         (i 0))
    (mapcar (lambda (name)
              (cl-incf i)
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
                                (when-let (workspace (+workspace-get name t))
                                  (+workspace-contains-buffer-p buf workspace)))))))
            (+workspace-list-names))))

(autoload 'consult--multi "consult")
;;;###autoload
(defun +vertico/switch-workspace-buffer (&optional force-same-workspace)
  "Switch to another buffer in the same workspace.

Type the workspace's number (starting from 1) followed by a space to display its
buffer list. Selecting a buffer in another workspace will switch to that
workspace instead. If FORCE-SAME-WORKSPACE (the prefix arg) is non-nil, that
buffer will be opened in the current workspace instead."
  (interactive "P")
  (when-let (buffer (consult--multi (+vertico--workspace-generate-sources)
                                    :require-match
                                    (confirm-nonexistent-file-or-buffer)
                                    :prompt (format "Switch to buffer (%s): "
                                                    (+workspace-current-name))
                                    :history 'consult--buffer-history
                                    :sort nil))
    (let ((origin-workspace (plist-get (cdr buffer) :name)))
      ;; Switch to the workspace the buffer belongs to, maybe
      (if (or (equal origin-workspace (+workspace-current-name))
              force-same-workspace)
          (funcall consult--buffer-display (car buffer))
        (+workspace-switch origin-workspace)
        (message "Switched to %S workspace" origin-workspace)
        (if-let (window (get-buffer-window (car buffer)))
            (select-window window)
          (funcall consult--buffer-display (car buffer)))))))

;;;###autoload
(defun +vertico/embark-open-in-new-workspace (x)
  "Open X (a file) in a new workspace."
  (interactive)
  (+workspace/new)
  (find-file x))
