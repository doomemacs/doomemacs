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


;;;###package lsp-mode
;; HACK: lsp-mode over TRAMP doesn't work with direct async, so force it off for
;;   LSP stdio connections.
(defadvice! +tramp--disable-direct-async-for-lsp-stdio-a (plist)
  :filter-return #'lsp-stdio-connection
  (let ((connect-fn (plist-get plist :connect)))
    (plist-put plist :connect
               (lambda (&rest args)
                 (letf! ((#'tramp-direct-async-process-p #'ignore))
                   (apply connect-fn args))))))


;;;###package eglot
;; HACK: Same deal with eglot.
(defadvice! +tramp--disable-direct-async-for-eglot-a (fn &rest args)
  :around #'eglot--connect
  (letf! ((#'tramp-direct-async-process-p #'ignore))
    (apply fn args)))


;; See magit/magit#5220
(after! magit
  (setq magit-tramp-pipe-stty-settings 'pty))


;; PERF: Newer versions of TRAMP will use SSH connection sharing for much faster
;;   connections. These don’t require you to reenter your password each time you
;;   connect. The compile command disables this feature, so we want to turn it
;;   back on.
(after! (tramp compile)
  (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options))


;;
;;; Memoization

;; PERF: Calls over TRAMP are expensive, so reduce the number of calls by more
;;   aggressively caching some common data. Inspired by
;;   https://coredumped.dev/2025/06/18/making-tramp-go-brrrr.
(defun +tramp--memoize (key cache fn &rest args)
  "Memoize a value if the key is a remote path."
  (if (and key (file-remote-p key))
      (if-let* ((current (assoc key (symbol-value cache))))
          (cdr current)
        (let ((current (apply fn args)))
          (set cache (cons (cons key current) (symbol-value cache)))
          current))
    (apply fn args)))

;;;###package magit
(defvar +tramp--magit-toplevel-cache nil)
(defadvice! +tramp--memoized-magit-toplevel-a (orig &optional directory)
  :around #'magit-toplevel
  (+tramp--memoize (or directory default-directory)
                   '+tramp--magit-toplevel-cache orig directory))

;;;###package project
(defvar +tramp--project-current-cache nil)
(defadvice! +tramp--memoized-project-current (fn &optional prompt directory)
  :around #'project-current
  (+tramp--memoize (or directory
                       project-current-directory-override
                       default-directory)
                   '+tramp--project-current-cache fn prompt directory))

;;;###package vc-git
(defvar +tramp--vc-git-root-cache nil)
(defadvice! +tramp--memoized-vc-git-root-a (fn file)
  :around #'vc-git-root
  (let ((value
         (+tramp--memoize (file-name-directory file)
                          '+tramp--vc-git-root-cache fn file)))
    ;; sometimes vc-git-root returns nil even when there is a root there
    (unless (cdar +tramp--vc-git-root-cache)
      (setq +tramp--vc-git-root-cache (cdr +tramp--vc-git-root-cache)))
    value))
