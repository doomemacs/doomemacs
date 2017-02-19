;;; extra/upload/config.el

;; Uses `ssh-deploy'. Expects a .dir-locals.el file in your project and expects
;; `ssh-deploy-root-remote' to be defined there to tell Emacs where to upload
;; files to. Supports FTP and SSH. If unset, automatically ascertains
;; `ssh-deploy-root-local' using `doom-project-root'.
;;
;; Interactive versions of `ssh-deploy's functions are in autoload.el.

(@def-package ssh-deploy
  :commands (ssh-deploy-upload-handler
             ssh-deploy-upload-handler-forced
             ssh-deploy-diff-handler
             ssh-deploy-browse-remove-handler
             ssh-deploy-remote-changes-handler)
  :init
  ;; Maybe auto-upload on save
  (@add-hook 'after-save-hook
    (when (and (bound-and-true-p ssh-deploy-root-remote) ssh-deploy-on-explicit-save)
      (ssh-deploy-upload-handler)))

  ;; Maybe check for changes on open file (if possible)
  (@add-hook 'find-file-hook
    (when (bound-and-true-p ssh-deploy-root-remote)
      (unless ssh-deploy-root-local
        (setq ssh-deploy-root-local (doom-project-root)))
      (when ssh-deploy-automatically-detect-remote-changes
        (ssh-deploy-remote-changes-handler))))

  :config
  (setq ssh-deploy-revision-folder (concat doom-cache-dir "ssh-revisions/")
        ssh-deploy-on-explicit-save t
        ssh-deploy-automatically-detect-remote-changes t))

