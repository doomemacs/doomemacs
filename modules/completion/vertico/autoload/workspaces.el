;;; completion/vertico/autoload/workspaces.el -*- lexical-binding: t; -*-
;;;###if (featurep! :ui workspaces)

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
                :narrow   ,(nth i key-range)
                :category buffer
                :state    consult--buffer-state
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
(defun +vertico/switch-workspace-buffer ()
  "Switch to another buffer in the same workspace.

Type the workspace's number (starting from 1) followed by a space to display its
buffer list."
  (interactive)
  ;; FIXME Open buffers in other workspaces in their respective workspace
  (when-let (buffer (consult--multi (+vertico--workspace-generate-sources)
                                    :require-match
                                    (confirm-nonexistent-file-or-buffer)
                                    :prompt (format "Switch to buffer (%s): "
                                                    (+workspace-current-name))
                                    :history 'consult--buffer-history
                                    :sort nil))
    ;; When the buffer does not belong to a source,
    ;; create a new buffer with the name.
    (unless (cdr buffer)
      (funcall consult--buffer-display (car buffer)))))

;;;###autoload
(defun +vertico/embark-open-in-new-workspace (x)
  "Open X (a file) in a new workspace."
  (interactive)
  (+workspace/new)
  (find-file x))
