;;; tools/upload/autoload.el -*- lexical-binding: t; -*-

(defvar +upload--old-remote nil)
(defvar +upload--old-local nil)

;;;###autoload
(defun +upload/register-remote (remote)
  "Map the local buffer to a given REMOTE in the current buffer.

If REMOTE is nil, removes any local mapping, or restores the old values for
`ssh-deploy-root-local' and `ssh-deploy-root-remote' if one existed before this
command was used."
  (interactive
   (list (unless current-prefix-arg
             (expand-file-name (or (read-directory-name "Remote directory: ")
                                   (user-error "Aborted"))))))
  (let* ((buffer (or (buffer-base-buffer) (current-buffer)))
         (buffer-file (or (buffer-file-name buffer)
                          (user-error "Not in a file-visiting buffer")))
         (local (or ssh-deploy-root-local
                    (doom-project-root (expand-file-name buffer-file))
                    default-directory)))
    (if remote
        (progn
          (message "ssh-deploy: mapping %S to %S (in %S)"
                   local remote (buffer-name buffer))
          (unless (local-variable-p '+upload--old-remote)
            (setq-local +upload--old-local ssh-deploy-root-local
                        +upload--old-remote ssh-deploy-root-remote))
          (setq-local ssh-deploy-root-local local
                      ssh-deploy-root-remote remote))
      (if (or +upload--old-local
              +upload--old-remote)
          (message "ssh-deploy: reverting mapping to %s -> %s (in %S)"
                   +upload--old-local +upload--old-remote (buffer-name buffer))
        (message  "ssh-deploy: clearing mapping (in %S)" (buffer-name buffer)))
      (setq-local ssh-deploy-root-local +upload--old-local
                  ssh-deploy-root-remote +upload--old-remote)
      (mapc #'kill-local-variable '(+upload--old-local +upload--old-remote)))))

;;;###autoload
(defun +upload/unregister-all-remotes ()
  "Undo manual local->remote mappings created with `+upload/register-remote'."
  (interactive)
  (dolist (buf (doom-buffer-list))
    (when (and (buffer-live-p buf)
               (local-variable-p '+upload--old-local buf))
      (with-current-buffer buf
        (+upload/register-remote nil)))))
