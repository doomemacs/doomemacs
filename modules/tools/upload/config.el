;;; tools/upload/config.el -*- lexical-binding: t; -*-

;; Uses `ssh-deploy' to map a local folder to a remote one. Set
;; `ssh-deploy-root-remote' and `ssh-deploy-root-local' in a .dir-locals.el file
;; to establish this mapping.
;;
;; Example:
;;   ((nil . ((ssh-deploy-root-local . "/local/path/to/project")
;;            (ssh-deploy-root-remote . "/ssh:user@server:/remote/project/")
;;            (ssh-deploy-on-explicit-save . 1))))
;;
;; Note: `ssh-deploy-root-local' is optional, and will resort to
;; `doom-project-root' if unspecified.

(use-package! ssh-deploy
  :commands (ssh-deploy-upload-handler
             ssh-deploy-upload-handler-forced
             ssh-deploy-diff-handler
             ssh-deploy-browse-remote-handler
             ssh-deploy-remote-changes-handler)
  :init
  (setq ssh-deploy-revision-folder (concat doom-cache-dir "ssh-revisions/")
        ssh-deploy-on-explicit-save 1
        ssh-deploy-automatically-detect-remote-changes nil)

  ;; Make these safe as file-local variables
  (dolist (sym '((ssh-deploy-root-local . stringp)
                 (ssh-deploy-root-remote . stringp)
                 (ssh-deploy-script . functionp)
                 (ssh-deploy-on-explicit-save . booleanp)
                 (ssh-deploy-force-on-explicit-save . booleanp)
                 (ssh-deploy-async . booleanp)
                 (ssh-deploy-exclude-list . listp)))
    (put (car sym) 'safe-local-variable (cdr sym)))

  ;; Maybe auto-upload on save
  (add-hook! 'after-save-hook
    (defun +upload-init-after-save-h ()
      (when (and (bound-and-true-p ssh-deploy-root-remote)
                 (integerp ssh-deploy-on-explicit-save)
                 (> ssh-deploy-on-explicit-save 0))
        (ssh-deploy-upload-handler ssh-deploy-force-on-explicit-save))))

  ;; Enable ssh-deploy if variables are set, and check for changes on open file
  ;; (if possible)
  (add-hook! 'find-file-hook
    (defun +upload-init-find-file-h ()
      (when (bound-and-true-p ssh-deploy-root-remote)
        (require 'ssh-deploy)
        (unless ssh-deploy-root-local
          (setq ssh-deploy-root-local (doom-project-root)))
        (when ssh-deploy-automatically-detect-remote-changes
          (ssh-deploy-remote-changes-handler)))))

  (defvar +upload-local-mappings nil
    "Global alist to store local to remote mappings (local-file . remote-path).
Each entry maps an absolute local file path to its corresponding remote path
for ssh-deploy functionality.")

  (defun +upload-clear-mappings ()
    "Clear all ssh-deploy mappings and remove buffer-local variables.
Iterates through all stored mappings in +upload-local-mappings and clears
ssh-deploy buffer-local variables for any open buffers, then clears the
global mapping list."
    (interactive)
    ;; For each mapping, find open buffers and clear their local variables
    (dolist (mapping +upload-local-mappings)
      (let* ((local-file (car mapping))
             (buffer (get-file-buffer local-file)))
        (when buffer
          (message "Unregistering ssh-deploy mapping from %s" buffer)
          (with-current-buffer buffer
            (setq-local ssh-deploy-root-local nil
                        ssh-deploy-root-remote nil)))))
    ;; Clear the global alist
    (setq +upload-local-mappings nil))

  (defun +upload-register-mapping (&optional remote-path)
    "Register or unregister ssh-deploy mapping for current buffer.
With C-u prefix, unregisters the current buffer's mapping and removes it
from the global +upload-local-mappings list. Otherwise prompts for REMOTE-PATH
and registers the mapping, storing it in both buffer-local variables and the
global mapping list. Updates or replaces any existing mapping for the current file."
    (interactive (if current-prefix-arg
                     (list nil)  ; Don't prompt when unregistering
                   (list (expand-file-name (read-file-name "Remote path: ")))))
    (require 'ssh-deploy)
    (let ((local-file (expand-file-name (buffer-file-name))))
      (if current-prefix-arg
          (progn
            (message "Unregistering ssh-deploy for this buffer")
            (setq-local ssh-deploy-root-local nil
                        ssh-deploy-root-remote nil)
            ;; Remove mapping from global alist
            (setq +upload-local-mappings
                  (assoc-delete-all local-file +upload-local-mappings)))
        (progn
          (setq-local ssh-deploy-root-local local-file
                      ssh-deploy-root-remote remote-path)
          (message "registered ssh-deploy for this buffer to %s" ssh-deploy-root-remote)
          ;; Add/update mapping in global alist
          (setq +upload-local-mappings
                (cons (cons local-file remote-path)
                      (assoc-delete-all local-file +upload-local-mappings))))))))
