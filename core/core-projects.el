;;; core-projects.el -*- lexical-binding: t; -*-

(defvar doom-projectile-cache-limit 25000
  "If any project cache surpasses this many files it is purged when quitting
Emacs.")

(defvar doom-projectile-cache-blacklist '("~" "/tmp" "/")
  "Directories that should never be cached.")

(defvar doom-projectile-cache-purge-non-projects nil
  "If non-nil, non-projects are purged from the cache on `kill-emacs-hook'.")

(defvar doom-projectile-fd-binary
  (or (cl-find-if #'executable-find '("fdfind" "fd"))
      "fd")
  "name of `fd-find' executable binary")


;;
;;; Packages

(use-package! projectile
  :after-call after-find-file dired-before-readin-hook minibuffer-setup-hook
  :commands (projectile-project-root
             projectile-project-name
             projectile-project-p
             projectile-locate-dominating-file)
  :init
  (setq projectile-cache-file (concat doom-cache-dir "projectile.cache")
        projectile-enable-caching doom-interactive-mode
        projectile-globally-ignored-files '(".DS_Store" "Icon" "TAGS")
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
        projectile-kill-buffers-filter 'kill-only-files
        projectile-known-projects-file (concat doom-cache-dir "projectile.projects")
        projectile-ignored-projects '("~/" "/tmp"))

  (global-set-key [remap evil-jump-to-tag] #'projectile-find-tag)
  (global-set-key [remap find-tag]         #'projectile-find-tag)

  :config
  (projectile-mode +1)

  ;; REVIEW Resolve the project root once, when the file/buffer is opened. This
  ;;        speeds up projectile's project root resolution by leaps, but does
  ;;        put you at risk of having a stale project root.
  (setq-hook! '(after-change-major-mode-hook
                ;; In case the user saves the file to a new location
                after-save-hook
                ;; ...or makes external changes then returns to Emacs
                focus-in-hook)
    projectile-project-root (if default-directory (doom-project-root)))

  ;; Projectile runs four functions to determine the root (in this order):
  ;;
  ;; + `projectile-root-local' -> checks the `projectile-project-root' variable
  ;;    for an explicit path.
  ;; + `projectile-root-bottom-up' -> searches from / to your current directory
  ;;   for the paths listed in `projectile-project-root-files-bottom-up'. This
  ;;   includes .git and .project
  ;; + `projectile-root-top-down' -> searches from the current directory down to
  ;;   / the paths listed in `projectile-root-files', like package.json,
  ;;   setup.py, or Cargo.toml
  ;; + `projectile-root-top-down-recurring' -> searches from the current
  ;;   directory down to / for a directory that has one of
  ;;   `projectile-project-root-files-top-down-recurring' but doesn't have a
  ;;   parent directory with the same file.
  ;;
  ;; In the interest of performance, we reduce the number of project root marker
  ;; files/directories projectile searches for when resolving the project root.
  (setq projectile-project-root-files-bottom-up
        (append '(".project"     ; doom project marker
                  ".git")        ; Git VCS root dir
                (when (executable-find "hg")
                  '(".hg"))      ; Mercurial VCS root dir
                (when (executable-find "bzr")
                  '(".bzr")))    ; Bazaar VCS root dir
        ;; This will be filled by other modules. We build this list manually so
        ;; projectile doesn't perform so many file checks every time it resolves
        ;; a project's root -- particularly when a file has no project.
        projectile-project-root-files '()
        projectile-project-root-files-top-down-recurring '("Makefile"))

  (push (abbreviate-file-name doom-local-dir) projectile-globally-ignored-directories)

  ;; Override projectile's dirconfig file '.projectile' with doom's project marker '.project'.
  (defadvice! doom--projectile-dirconfig-file-a ()
    :override #'projectile-dirconfig-file
    (cond
     ;; Prefers '.projectile' to maintain compatibility with existing projects.
     ((file-exists-p! (or ".projectile" ".project") (projectile-project-root)))
     ((expand-file-name ".project" (projectile-project-root)))))

  ;; Disable commands that won't work, as is, and that Doom already provides a
  ;; better alternative for.
  (put 'projectile-ag 'disabled "Use +{ivy,helm}/project-search instead")
  (put 'projectile-ripgrep 'disabled "Use +{ivy,helm}/project-search instead")
  (put 'projectile-grep 'disabled "Use +{ivy,helm}/project-search instead")

  ;; Treat current directory in dired as a "file in a project" and track it
  (add-hook 'dired-before-readin-hook #'projectile-track-known-projects-find-file-hook)

  ;; Accidentally indexing big directories like $HOME or / will massively bloat
  ;; projectile's cache (into the hundreds of MBs). This purges those entries
  ;; when exiting Emacs to prevent slowdowns/freezing when cache files are
  ;; loaded or written to.
  (add-hook! 'kill-emacs-hook
    (defun doom-cleanup-project-cache-h ()
      "Purge projectile cache entries that:

a) have too many files (see `doom-projectile-cache-limit'),
b) represent blacklisted directories that are too big, change too often or are
   private. (see `doom-projectile-cache-blacklist'),
c) are not valid projectile projects."
      (when (and (bound-and-true-p projectile-projects-cache)
                 doom-interactive-mode)
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
        (projectile-serialize-cache))))

  ;; It breaks projectile's project root resolution if HOME is a project (e.g.
  ;; it's a git repo). In that case, we disable bottom-up root searching to
  ;; prevent issues. This makes project resolution a little slower and less
  ;; accurate in some cases.
  (let ((default-directory "~"))
    (when (cl-find-if #'projectile-file-exists-p
                      projectile-project-root-files-bottom-up)
      (doom-log "HOME appears to be a project. Disabling bottom-up root search.")
      (setq projectile-project-root-files
            (append projectile-project-root-files-bottom-up
                    projectile-project-root-files)
            projectile-project-root-files-bottom-up nil)))

  (cond
   ;; If fd exists, use it for git and generic projects. fd is a rust program
   ;; that is significantly faster than git ls-files or find, and it respects
   ;; .gitignore. This is recommended in the projectile docs.
   ((executable-find doom-projectile-fd-binary)
    (setq projectile-generic-command
          (format "%s . --color=never --type f -0 -H -E .git"
                  doom-projectile-fd-binary)
          projectile-git-command projectile-generic-command
          projectile-git-submodule-command nil
          ;; ensure Windows users get fd's benefits
          projectile-indexing-method 'alien))

   ;; Otherwise, resort to ripgrep, which is also faster than find
   ((executable-find "rg")
    (setq projectile-generic-command
          (concat "rg -0 --files --color=never --hidden"
                  (cl-loop for dir in projectile-globally-ignored-directories
                           concat (format " --glob '!%s'" dir)))
          projectile-git-command projectile-generic-command
          projectile-git-submodule-command nil
          ;; ensure Windows users get rg's benefits
          projectile-indexing-method 'alien))

   ;; Fix breakage on windows in git projects with submodules, since Windows
   ;; doesn't have tr
   (IS-WINDOWS
    (setq projectile-git-submodule-command nil)))

  (defadvice! doom--projectile-default-generic-command-a (orig-fn &rest args)
    "If projectile can't tell what kind of project you're in, it issues an error
when using many of projectile's command, e.g. `projectile-compile-command',
`projectile-run-project', `projectile-test-project', and
`projectile-configure-project', for instance.

This suppresses the error so these commands will still run, but prompt you for
the command instead."
    :around #'projectile-default-generic-command
    (ignore-errors (apply orig-fn args)))

  ;; Projectile root-searching functions can cause an infinite loop on TRAMP
  ;; connections, so disable them.
  ;; TODO Is this still necessary?
  (defadvice! doom--projectile-locate-dominating-file-a (file _name)
    "Don't traverse the file system if on a remote connection."
    :before-while #'projectile-locate-dominating-file
    (and (stringp file)
         (not (file-remote-p file nil t)))))


;;
;;; Project-based minor modes

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
  "Define a project minor mode named NAME and where/how it is activated.

Project modes allow you to configure 'sub-modes' for major-modes that are
specific to a folder, project structure, framework or whatever arbitrary context
you define. These project modes can have their own settings, keymaps, hooks,
snippets, etc.

This creates NAME-hook and NAME-map as well.

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
    (macroexp-progn
     (append
      (when on-load
        `((defvar ,init-var nil)))
      `((define-minor-mode ,name
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
        (dolist (hook ,add-hooks)
          (add-hook ',(intern (format "%s-hook" name)) hook)))
      (cond ((or files modes when)
             (cl-check-type files (or null list string))
             (let ((fn
                    `(lambda ()
                       (and (not (bound-and-true-p ,name))
                            (and buffer-file-name (not (file-remote-p buffer-file-name nil t)))
                            ,(or (null match)
                                 `(if buffer-file-name (string-match-p ,match buffer-file-name)))
                            ,(or (null files)
                                 ;; Wrap this in `eval' to prevent eager expansion
                                 ;; of `project-file-exists-p!' from pulling in
                                 ;; autoloaded files prematurely.
                                 `(eval
                                   '(project-file-exists-p!
                                     ,(if (stringp (car files)) (cons 'and files) files))))
                            ,(or when t)
                            (,name 1)))))
               (if modes
                   `((dolist (mode ,modes)
                       (let ((hook-name
                              (intern (format "doom--enable-%s%s-h" ',name
                                              (if (eq mode t) "" (format "-in-%s" mode))))))
                         (fset hook-name #',fn)
                         (if (eq mode t)
                             (add-to-list 'auto-minor-mode-magic-alist (cons hook-name #',name))
                           (add-hook (intern (format "%s-hook" mode)) hook-name)))))
                 `((add-hook 'change-major-mode-after-body-hook #',fn)))))
            (match
             `((add-to-list 'auto-minor-mode-alist (cons ,match #',name)))))))))

(provide 'core-projects)
;;; core-projects.el ends here
