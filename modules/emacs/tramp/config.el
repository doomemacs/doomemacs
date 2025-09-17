;;; emacs/tramp/config.el -*- lexical-binding: t; -*-

;; Prefix tramp autosaves to prevent conflicts with local ones
(cl-pushnew (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                  (concat auto-save-list-file-prefix "tramp-\\2") t)
            auto-save-file-name-transforms
            :test #'equal)


(after! tramp
  (setq remote-file-name-inhibit-cache 60
        remote-file-name-inhibit-locks t
        remote-file-name-inhibit-auto-save-visited t
        tramp-copy-size-limit (* 1024 1024) ; 1mb
        tramp-use-scp-direct-remote-copying t
        tramp-completion-reread-directory-timeout 60
        tramp-backup-directory-alist backup-directory-alist
        tramp-auto-save-directory  (concat doom-cache-dir "tramp-autosave/"))

  (unless (featurep :system 'windows)
    (setq tramp-default-method "ssh")))  ; faster than scp on Windows


;; See https://coredumped.dev/2025/06/18/making-tramp-go-brrrr.
(after! files-x
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))

  (connection-local-set-profiles
   `(:application tramp :protocol "scp")
   'remote-direct-async-process))


;; See magit/magit#5220
(after! magit
  (setq magit-tramp-pipe-stty-settings 'pty))


;; PERF: Newer versions of TRAMP will use SSH connection sharing for much faster
;;   connections. These don’t require you to reenter your password each time you
;;   connect. The compile command disables this feature, so we want to turn it
;;   back on.
(after! (tramp compile)
  (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options))
