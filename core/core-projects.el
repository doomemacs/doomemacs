;;; core-projects.el -*- lexical-binding: t; -*-

(defvar doom-projectile-cache-limit 25000
  "If any project cache surpasses this many files it is purged when quitting
Emacs.")

(defvar doom-projectile-cache-blacklist '("~" "/tmp" "/")
  "Directories that should never be cached.")

(defvar doom-projectile-cache-purge-non-projects nil
  "If non-nil, non-projects are purged from the cache on `kill-emacs-hook'.")

(defvar doom-projectile-fd-binary "fd"
  "name of `fd-find' executable binary")

;;
;;; Packages

(def-package! projectile
  :after-call (after-find-file dired-before-readin-hook minibuffer-setup-hook)
  :commands (projectile-project-root
             projectile-project-name
             projectile-project-p
             projectile-add-known-project) ; TODO PR autoload upstream
  :init
  (setq projectile-cache-file (concat doom-cache-dir "projectile.cache")
        projectile-enable-caching (not noninteractive)
        projectile-known-projects-file (concat doom-cache-dir "projectile.projects")
        projectile-require-project-root t
        projectile-globally-ignored-files '(".DS_Store" "Icon" "TAGS")
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
        projectile-ignored-projects '("~/" "/tmp")
        projectile-kill-buffers-filter 'kill-only-files
        projectile-files-cache-expire 604800 ; expire after a week
        projectile-sort-order 'recentf
        projectile-use-git-grep t) ; use git-grep for text searches

  :config
  (add-hook 'dired-before-readin-hook #'projectile-track-known-projects-find-file-hook)
  (projectile-mode +1)

  (global-set-key [remap evil-jump-to-tag] #'projectile-find-tag)
  (global-set-key [remap find-tag]         #'projectile-find-tag)

  ;; a more generic project root file
  (push ".project" projectile-project-root-files-bottom-up)
  (push (abbreviate-file-name doom-local-dir) projectile-globally-ignored-directories)

  ;; Accidentally indexing big directories like $HOME or / will massively bloat
  ;; projectile's cache (into the hundreds of MBs). This purges those entries
  ;; when exiting Emacs to prevent slowdowns/freezing when cache files are
  ;; loaded or written to.
  (defun doom|cleanup-project-cache ()
    "Purge projectile cache entries that:

a) have too many files (see `doom-projectile-cache-limit'),
b) represent blacklisted directories that are too big, change too often or are
   private. (see `doom-projectile-cache-blacklist'),
c) are not valid projectile projects."
    (when (bound-and-true-p projectile-projects-cache)
      (cl-loop with blacklist = (mapcar #'file-truename doom-projectile-cache-blacklist)
               for proot in (hash-table-keys projectile-projects-cache)
               if (or (not (stringp proot))
                      (>= (length (gethash proot projectile-projects-cache))
                          doom-projectile-cache-limit)
                      (member (substring proot 0 -1) blacklist)
                      (and doom-projectile-cache-purge-non-projects
                           (not (doom-project-p proot))))
               do (doom-log "Removed %S from projectile cache" proot)
               and do (remhash proot projectile-projects-cache)
               and do (remhash proot projectile-projects-cache-time)
               and do (remhash proot projectile-project-type-cache))
      (projectile-serialize-cache)))
  (unless noninteractive
    (add-hook 'kill-emacs-hook #'doom|cleanup-project-cache))

  ;; It breaks projectile's project root resolution if HOME is a project (e.g.
  ;; it's a git repo). In that case, we disable bottom-up root searching to
  ;; prevent issues. This makes project resolution a little slower and less
  ;; accurate in some cases.
  (let ((default-directory "~"))
    (when (cl-find-if #'projectile-file-exists-p
                      projectile-project-root-files-bottom-up)
      (message "HOME appears to be a project. Disabling bottom-up root search.")
      (setq projectile-project-root-files
            (append projectile-project-root-files-bottom-up
                    projectile-project-root-files)
            projectile-project-root-files-bottom-up nil)))

  ;; Projectile root-searching functions can cause an infinite loop on TRAMP
  ;; connections, so disable them.
  ;; TODO Is this still necessary?
  (defun doom*projectile-locate-dominating-file (orig-fn file name)
    "Don't traverse the file system if on a remote connection."
    (when (and (stringp file)
               (not (file-remote-p file)))
      (funcall orig-fn file name)))
  (advice-add #'projectile-locate-dominating-file :around #'doom*projectile-locate-dominating-file)

  (cond
   ;; If fd exists, use it for git and generic projects. fd is a rust program
   ;; that is significantly faster and respects .gitignore. This is recommended
   ;; in the projectile docs
   ((executable-find doom-projectile-fd-binary)
    (setq projectile-git-command (concat
                                  doom-projectile-fd-binary
                                  " . --type f -0 -H -E .git")
          projectile-generic-command projectile-git-command))

   ;; Otherwise, resort to ripgrep, which is also faster than find.
   ((executable-find "rg")
    (setq projectile-generic-command
          (concat "rg -0 --files --color=never --hidden"
                  (cl-loop for dir in projectile-globally-ignored-directories
                           concat (format " --glob '!%s'" dir))))
    (when IS-WINDOWS
      (setq projectile-indexing-method 'alien
            projectile-enable-caching nil)))))

;;
;; Project-based minor modes

(defvar doom-project-hook nil
  "Hook run when a project is enabled. The name of the project's mode and its
state are passed in.")

(cl-defmacro def-project-mode! (name &key
                                     modes
                                     files
                                     when
                                     match
                                     add-hooks
                                     on-load
                                     on-enter
                                     on-exit)
  "Define a project minor-mode named NAME (a symbol) and declare where and how
it is activated. Project modes allow you to configure 'sub-modes' for
major-modes that are specific to a folder, project structure, framework or
whatever arbitrary context you define. These project modes can have their own
settings, keymaps, hooks, snippets, etc.

This creates NAME-hook and NAME-map as well.

A project can be enabled through .dir-locals.el too, by setting `doom-project'.

PLIST may contain any of these properties, which are all checked to see if NAME
should be activated. If they are *all* true, NAME is activated.

  :modes MODES -- if buffers are derived from MODES (one or a list of symbols).

  :files FILES -- if project contains FILES; takes a string or a form comprised
    of nested (and ...) and/or (or ...) forms. Each path is relative to the
    project root, however, if prefixed with a '.' or '..', it is relative to the
    current buffer.

  :match REGEXP -- if file name matches REGEXP

  :when PREDICATE -- if PREDICATE returns true (can be a form or the symbol of a
    function)

  :add-hooks HOOKS -- HOOKS is a list of hooks to add this mode's hook.

  :on-load FORM -- FORM to run the first time this project mode is enabled.

  :on-enter FORM -- FORM is run each time the mode is activated.

  :on-exit FORM -- FORM is run each time the mode is disabled.

Relevant: `doom-project-hook'."
  (declare (indent 1))
  (let ((init-var (intern (format "%s-init" name))))
    `(progn
       ,(if on-load `(defvar ,init-var nil))
       (define-minor-mode ,name
         "A project minor mode generated by `def-project-mode!'."
         :init-value nil
         :lighter ""
         :keymap (make-sparse-keymap)
         (if (not ,name)
             ,on-exit
           (run-hook-with-args 'doom-project-hook ',name ,name)
           ,(when on-load
              `(unless ,init-var
                 ,on-load
                 (setq ,init-var t)))
           ,on-enter))
       ,@(cl-loop for hook in add-hooks
                  collect `(add-hook ',(intern (format "%s-hook" name))
                                     #',hook))
       ,(when (or modes match files when)
          `(associate! ,name
             :modes ,modes
             :match ,match
             :files ,files
             :when ,when)))))

(provide 'core-projects)
;;; core-projects.el ends here
