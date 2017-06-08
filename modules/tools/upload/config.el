;;; tools/upload/config.el -*- lexical-binding: t; -*-

;; Uses `ssh-deploy' to map a local folder to a remote one. Use
;; `ssh-deploy-root-remote' and `ssh-deploy-root-local' to set up this mapping.
;;
;; Example:
;;   (setq ssh-deploy-root-local  "/home/hlissner/work/site/"
;;         ssh-deploy-root-remote "/ssh:hlissner@myserver.com:/var/www/site/"
;;         ssh-deploy-on-explicity-save t)
;;
;; Note: `ssh-deploy-root-local' is optional, and will resort to
;; `doom-project-root' if unspecified.
;;
;; Can be used via .dir-locals.el file in your project.

(def-package! ssh-deploy
  :commands (ssh-deploy-upload-handler
             ssh-deploy-upload-handler-forced
             ssh-deploy-diff-handler
             ssh-deploy-browse-remove-handler
             ssh-deploy-remote-changes-handler)
  :init
  ;; Maybe auto-upload on save
  (defun +upload|init-after-save ()
    (when (and (bound-and-true-p ssh-deploy-root-remote)
               ssh-deploy-on-explicit-save)
      (ssh-deploy-upload-handler)))
  (add-hook 'after-save-hook #'+upload|init-after-save)

  ;; Enable ssh-deploy if variables are set, and check for changes on open file
  ;; (if possible)
  (defun +upload|init-find-file ()
    (when (bound-and-true-p ssh-deploy-root-remote)
      (require 'ssh-deploy)
      (unless ssh-deploy-root-local
        (setq ssh-deploy-root-local (doom-project-root)))
      (when ssh-deploy-automatically-detect-remote-changes
        (ssh-deploy-remote-changes-handler))))
  (add-hook 'find-file-hook #'+upload|init-find-file)

  :config
  (setq ssh-deploy-revision-folder (concat doom-cache-dir "ssh-revisions/")
        ssh-deploy-on-explicit-save t
        ssh-deploy-automatically-detect-remote-changes t))

