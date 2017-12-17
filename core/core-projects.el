;;; core-projects.el -*- lexical-binding: t; -*-

(defvar doom-project-hook nil
  "Hook run when a project is enabled. The name of the project's mode and its
state are passed in.")

(def-package! projectile
  :init
  (setq projectile-cache-file (concat doom-cache-dir "projectile.cache")
        projectile-enable-caching (not noninteractive)
        projectile-indexing-method 'alien
        projectile-known-projects-file (concat doom-cache-dir "projectile.projects")
        projectile-require-project-root nil
        projectile-globally-ignored-files '(".DS_Store" "Icon" "TAGS")
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o"))

  (add-hook 'doom-init-hook #'projectile-mode)
  :config
  ;; a more generic project root file
  (push ".project" projectile-project-root-files-bottom-up)

  (nconc projectile-globally-ignored-directories (list (abbreviate-file-name doom-local-dir) ".sync"))
  (nconc projectile-other-file-alist '(("css"  . ("scss" "sass" "less" "style"))
                                       ("scss" . ("css"))
                                       ("sass" . ("css"))
                                       ("less" . ("css"))
                                       ("styl" . ("css"))))

  ;; Projectile root-searching functions can cause an infinite loop on TRAMP
  ;; connections, so disable them.
  (defun doom*projectile-locate-dominating-file (orig-fn &rest args)
    "Don't traverse the file system if on a remote connection."
    (unless (file-remote-p default-directory)
      (apply orig-fn args)))
  (advice-add #'projectile-locate-dominating-file :around #'doom*projectile-locate-dominating-file)

  (defun doom*projectile-cache-current-file (orig-fun &rest args)
    "Don't cache ignored files."
    (unless (cl-loop for path in (projectile-ignored-directories)
                     if (string-prefix-p buffer-file-name (expand-file-name path))
                     return t)
      (apply orig-fun args)))
  (advice-add #'projectile-cache-current-file :around #'doom*projectile-cache-current-file))


;;
;; Library
;;

(defun doom//reload-project ()
  "Reload the project root cache."
  (interactive)
  (projectile-invalidate-cache nil)
  (projectile-reset-cached-project-root)
  (dolist (fn projectile-project-root-files-functions)
    (remhash (format "%s-%s" fn default-directory) projectile-project-root-cache)))

(defun doom-project-p ()
  "Whether or not this buffer is currently in a project or not."
  (let ((projectile-require-project-root t))
    (projectile-project-p)))

(defun doom-project-root ()
  "Get the path to the root of your project.
If STRICT-P, return nil if no project was found, otherwise return
`default-directory'."
  (let (projectile-require-project-root)
    (projectile-project-root)))

(defalias 'doom-project-expand #'projectile-expand-root)

(defmacro doom-project-has! (files)
  "Checks if the project has the specified FILES.
Paths are relative to the project root, unless they start with ./ or ../ (in
which case they're relative to `default-directory'). If they start with a slash,
they are absolute."
  (doom--resolve-path-forms files (doom-project-root)))


;;
;; Projects
;;

(defvar-local doom-project nil
  "A list of project mode to enable. Used for .dir-locals.el.")

(defun doom|autoload-project-mode ()
  "Auto-enable projects listed in `doom-project', which is meant to be set from
.dir-locals.el files."
  (cl-loop for mode in doom-project
           unless (symbol-value mode)
           do (funcall mode)))
(add-hook 'after-change-major-mode-hook #'doom|autoload-project-mode)

(defmacro def-project-mode! (name &rest plist)
  "Define a project minor-mode named NAME (a symbol) and declare where and how
it is activated. Project modes allow you to configure 'sub-modes' for
major-modes that are specific to a specific folder, certain project structure,
framework or arbitrary context you define. These project modes can have their
own settings, keymaps, hooks, snippets, etc.

This creates NAME-hook and NAME-map as well.

A project can be enabled through .dir-locals.el too, if `doom-project' is set to
the name (symbol) of the project mode(s) to enable.

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
  (declare (indent 1) (doc-string 2))
  (let ((doc-string (if (stringp (car plist))
                        (prog1 (car plist)
                          (setq plist (cdr plist)))
                      "A project minor mode."))
        (modes (plist-get plist :modes))
        (files (plist-get plist :files))
        (when  (plist-get plist :when))
        (match (plist-get plist :match))
        (hooks (plist-get plist :add-hooks))
        (load-form  (plist-get plist :on-load))
        (enter-form (plist-get plist :on-enter))
        (exit-form  (plist-get plist :on-exit))
        (init-var (intern (format "%s-init" name))))
    `(progn
       ,(if load-form `(defvar ,init-var nil))
       (define-minor-mode ,name
         ,doc-string
         :init-value nil
         :lighter ""
         :keymap (make-sparse-keymap)
         (if (not ,name)
             ,exit-form
           (run-hook-with-args 'doom-project-hook ',name)
           ,(when load-form
              `(unless ,init-var
                 ,load-form
                 (setq ,init-var t)))
           ,enter-form))
       ,(when hooks
          `(setq ,(intern (format "%s-hook" name)) ',hooks))
       ,(when (or modes match files when)
          `(associate! ,name
             :modes ,modes
             :match ,match
             :files ,files
             :when ,when)))))

(provide 'core-projects)
;;; core-projects.el ends here
