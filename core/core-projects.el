;;; core-projects.el -*- lexical-binding: t; -*-

(def-package! projectile
  :after-call (pre-command-hook after-find-file dired-before-readin-hook)
  :commands (projectile-project-root projectile-project-name projectile-project-p)
  :init
  (setq projectile-cache-file (concat doom-cache-dir "projectile.cache")
        projectile-enable-caching (not noninteractive)
        projectile-known-projects-file (concat doom-cache-dir "projectile.projects")
        projectile-require-project-root t
        projectile-globally-ignored-files '(".DS_Store" "Icon" "TAGS")
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
        projectile-ignored-projects '("~/" "/tmp")
        projectile-kill-buffers-filter 'kill-only-files)

  :config
  (add-hook 'dired-before-readin-hook #'projectile-track-known-projects-find-file-hook)
  (add-hook 'find-file-hook #'doom|init-project-mode)
  (projectile-mode +1)

  ;; a more generic project root file
  (push ".project" projectile-project-root-files-bottom-up)

  (setq projectile-globally-ignored-directories
        (append projectile-globally-ignored-directories
                (list (abbreviate-file-name doom-local-dir) ".sync"
                      "node_modules" "flow-typed"))
        projectile-other-file-alist
        (append projectile-other-file-alist
                '(("css"  "scss" "sass" "less" "styl")
                  ("scss" "css")
                  ("sass" "css")
                  ("less" "css")
                  ("styl" "css"))))

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
    (unless (file-remote-p default-directory)
      (funcall orig-fn file name)))
  (advice-add #'projectile-locate-dominating-file :around #'doom*projectile-locate-dominating-file)

  ;; If fd exists, use it for git and generic projects
  ;; fd is a rust program that is significantly faster. It also respects
  ;; .gitignore. This is recommended in the projectile docs
  (when (executable-find "fd")
    (setq projectile-git-command "fd . --type f -0"
          projectile-generic-command projectile-git-command)))


;;
;; Project-based minor modes

(defvar-local doom-project nil
  "Either the symbol or a list of project modes you want to enable. Available
for .dir-locals.el.")

(defvar doom-project-hook nil
  "Hook run when a project is enabled. The name of the project's mode and its
state are passed in.")

(defun doom|init-project-mode ()
  "Auto-enable the project(s) listed in `doom-project'."
  (when doom-project
    (if (symbolp doom-project)
        (funcall doom-project)
      (cl-loop for mode in doom-project
               unless (symbol-value mode)
               do (funcall mode)))))

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
major-modes that are specific to a specific folder, certain project structure,
framework or arbitrary context you define. These project modes can have their
own settings, keymaps, hooks, snippets, etc.

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
