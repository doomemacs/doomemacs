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
          (ssh-deploy-remote-changes-handler))))))
