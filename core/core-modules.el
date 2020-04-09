;;; core-modules.el --- module & package management system -*- lexical-binding: t; -*-

(defvar doom-init-modules-p nil
  "Non-nil if `doom-initialize-modules' has run.")

(defvar doom-modules ()
  "A hash table of enabled modules. Set by `doom-initialize-modules'.")

(defvar doom-modules-dirs
  (list (expand-file-name "modules/" doom-private-dir)
        doom-modules-dir)
  "A list of module root directories. Order determines priority.")

(defconst doom-obsolete-modules
  '((:feature (version-control (:emacs vc) (:ui vc-gutter))
              (spellcheck (:checkers spell))
              (syntax-checker (:checkers syntax))
              (evil (:editor evil))
              (snippets (:editor snippets))
              (file-templates (:editor file-templates))
              (workspaces (:ui workspaces))
              (eval (:tools eval))
              (lookup (:tools lookup))
              (debugger (:tools debugger)))
    (:tools (rotate-text (:editor rotate-text))
            (vterm (:term vterm))
            (password-store (:tools pass))
            (flycheck (:checkers syntax))
            (flyspell (:checkers spell)))
    (:emacs (electric-indent (:emacs electric))
            (hideshow (:editor fold))
            (eshell (:term eshell))
            (term (:term term)))
    (:ui (doom-modeline (:ui modeline))
         (fci (:ui fill-column))
         (evil-goggles (:ui ophints))
         (tabbar (:ui tabs)))
    (:app (email (:email mu4e))
          (notmuch (:email notmuch))))
  "A tree alist that maps deprecated modules to their replacement(s).

Each entry is a three-level tree. For example:

  (:feature (version-control (:emacs vc) (:ui vc-gutter))
            (spellcheck (:checkers spell))
            (syntax-checker (:tools flycheck)))

This marks :feature version-control, :feature spellcheck and :feature
syntax-checker modules obsolete. e.g. If :feature version-control is found in
your `doom!' block, a warning is emitted before replacing it with :emacs vc and
:ui vc-gutter.")

(defvar doom-inhibit-module-warnings doom-interactive-mode
  "If non-nil, don't emit deprecated or missing module warnings at startup.")

;;; Custom hooks
(defvar doom-before-init-modules-hook nil
  "A list of hooks to run before Doom's modules' config.el files are loaded, but
after their init.el files are loaded.")

(defvar doom-init-modules-hook nil
  "A list of hooks to run after Doom's modules' config.el files have loaded, but
before the user's private module.")

(defvaralias 'doom-after-init-modules-hook 'after-init-hook)

(defvar doom--current-module nil)
(defvar doom--current-flags nil)


;;
;;; Bootstrap API

(defun doom-initialize-modules (&optional force-p)
  "Loads the init.el in `doom-private-dir' and sets up hooks for a healthy
session of Dooming. Will noop if used more than once, unless FORCE-P is
non-nil."
  (when (or force-p (not doom-init-modules-p))
    (setq doom-init-modules-p t
          doom-modules nil)
    (load custom-file 'noerror 'nomessage)
    (when (load! "init" doom-private-dir t)
      (when doom-modules
        (maphash (lambda (key plist)
                   (let ((doom--current-module key)
                         (doom--current-flags (plist-get plist :flags)))
                     (load! "init" (plist-get plist :path) t)))
                 doom-modules))
      (run-hook-wrapped 'doom-before-init-modules-hook #'doom-try-run-hook)
      (when doom-interactive-mode
        (when doom-modules
          (maphash (lambda (key plist)
                     (let ((doom--current-module key)
                           (doom--current-flags (plist-get plist :flags)))
                       (load! "config" (plist-get plist :path) t)))
                   doom-modules))
        (run-hook-wrapped 'doom-init-modules-hook #'doom-try-run-hook)
        (load! "config" doom-private-dir t)))))


;;
;;; Module API

(defun doom-module-p (category module &optional flag)
  "Returns t if CATEGORY MODULE is enabled (ie. present in `doom-modules')."
  (declare (pure t) (side-effect-free t))
  (when-let (plist (gethash (cons category module) doom-modules))
    (or (null flag)
        (and (memq flag (plist-get plist :flags))
             t))))

(defun doom-module-get (category module &optional property)
  "Returns the plist for CATEGORY MODULE. Gets PROPERTY, specifically, if set."
  (declare (pure t) (side-effect-free t))
  (when-let (plist (gethash (cons category module) doom-modules))
    (if property
        (plist-get plist property)
      plist)))

(defun doom-module-put (category module &rest plist)
  "Set a PROPERTY for CATEGORY MODULE to VALUE. PLIST should be additional pairs
of PROPERTY and VALUEs.

\(fn CATEGORY MODULE PROPERTY VALUE &rest [PROPERTY VALUE [...]])"
  (if-let (old-plist (doom-module-get category module))
      (progn
        (when plist
          (when (cl-oddp (length plist))
            (signal 'wrong-number-of-arguments (list (length plist))))
          (while plist
            (plist-put old-plist (pop plist) (pop plist))))
        (puthash (cons category module) old-plist doom-modules))
    (puthash (cons category module) plist doom-modules)))

(defun doom-module-set (category module &rest plist)
  "Enables a module by adding it to `doom-modules'.

CATEGORY is a keyword, module is a symbol, PLIST is a plist that accepts the
following properties:

  :flags [SYMBOL LIST]  list of enabled category flags
  :path  [STRING]       path to category root directory

Example:
  (doom-module-set :lang 'haskell :flags '(+intero))"
  (puthash (cons category module)
           plist
           doom-modules))

(defun doom-module-path (category module &optional file)
  "Like `expand-file-name', but expands FILE relative to CATEGORY (keywordp) and
MODULE (symbol).

If the category isn't enabled this will always return nil. For finding disabled
modules use `doom-module-locate-path'."
  (let ((path (doom-module-get category module :path)))
    (if file
        (let (file-name-handler-alist)
          (expand-file-name file path))
      path)))

(defun doom-module-locate-path (category &optional module file)
  "Searches `doom-modules-dirs' to find the path to a module.

CATEGORY is a keyword (e.g. :lang) and MODULE is a symbol (e.g. 'python). FILE
is a string that will be appended to the resulting path. If no path exists, this
returns nil, otherwise an absolute path.

This doesn't require modules to be enabled. For enabled modules us
`doom-module-path'."
  (when (keywordp category)
    (setq category (doom-keyword-name category)))
  (when (and module (symbolp module))
    (setq module (symbol-name module)))
  (cl-loop with file-name-handler-alist = nil
           for default-directory in doom-modules-dirs
           for path = (concat category "/" module "/" file)
           if (file-exists-p path)
           return (expand-file-name path)))

(defun doom-module-from-path (&optional path enabled-only)
  "Returns a cons cell (CATEGORY . MODULE) derived from PATH (a file path).
If ENABLED-ONLY, return nil if the containing module isn't enabled."
  (if (null path)
      (if doom--current-module
          (if enabled-only
              (and (doom-module-p (car doom--current-module)
                                  (cdr doom--current-module))
                   doom--current-module)
            doom--current-module)
        (ignore-errors
          (doom-module-from-path (file!))))
    (let* ((file-name-handler-alist nil)
           (path (file-truename (or path (file!)))))
      (save-match-data
        (cond ((string-match "/modules/\\([^/]+\\)/\\([^/]+\\)\\(?:/.*\\)?$" path)
               (when-let* ((category (doom-keyword-intern (match-string 1 path)))
                           (module   (intern (match-string 2 path))))
                 (and (or (null enabled-only)
                          (doom-module-p category module))
                      (cons category module))))
              ((file-in-directory-p path doom-core-dir)
               (cons :core (intern (file-name-base path))))
              ((file-in-directory-p path doom-private-dir)
               (cons :private (intern (file-name-base path)))))))))

(defun doom-module-load-path (&optional module-dirs)
  "Return a list of file paths to activated modules.

The list is in no particular order and its file paths are absolute. If
MODULE-DIRS is non-nil, include all modules (even disabled ones) available in
those directories. The first returned path is always `doom-private-dir'."
  (declare (pure t) (side-effect-free t))
  (append (list doom-private-dir)
          (if module-dirs
              (doom-files-in (if (listp module-dirs)
                                 module-dirs
                               doom-modules-dirs)
                             :type 'dirs
                             :mindepth 1
                             :depth 1)
            (cl-loop for plist being the hash-values of (doom-modules)
                     collect (plist-get plist :path)))
          nil))

(defun doom-modules (&optional refresh-p)
  "Minimally initialize `doom-modules' (a hash table) and return it.
This value is cached. If REFRESH-P, then don't use the cached value."
  (or (unless refresh-p doom-modules)
      (let (doom-interactive-mode
            doom-modules
            doom-init-modules-p)
        (load! "init" doom-private-dir t)
        (or doom-modules
            (make-hash-table :test 'equal
                             :size 20
                             :rehash-threshold 1.0)))))


;;
;;; Use-package modifications

(eval-and-compile
  (autoload 'use-package "use-package-core" nil nil t)

  (setq use-package-compute-statistics doom-debug-mode
        use-package-verbose doom-debug-mode
        use-package-minimum-reported-time (if doom-debug-mode 0 0.1)
        use-package-expand-minimally doom-interactive-mode))

(defvar doom--deferred-packages-alist '(t))

(with-eval-after-load 'use-package-core
  ;; Macros are already fontified, no need for this
  (font-lock-remove-keywords 'emacs-lisp-mode use-package-font-lock-keywords)

  ;; We define :minor and :magic-minor from the `auto-minor-mode' package here
  ;; so we don't have to load `auto-minor-mode' so early.
  (dolist (keyword '(:minor :magic-minor))
    (setq use-package-keywords
          (use-package-list-insert keyword use-package-keywords :commands)))

  (defalias 'use-package-normalize/:minor #'use-package-normalize-mode)
  (defun use-package-handler/:minor (name _ arg rest state)
    (use-package-handle-mode name 'auto-minor-mode-alist arg rest state))

  (defalias 'use-package-normalize/:magic-minor #'use-package-normalize-mode)
  (defun use-package-handler/:magic-minor (name _ arg rest state)
    (use-package-handle-mode name 'auto-minor-mode-magic-alist arg rest state))

  ;; Adds two keywords to `use-package' to expand its lazy-loading capabilities:
  ;;
  ;;   :after-call SYMBOL|LIST
  ;;   :defer-incrementally SYMBOL|LIST|t
  ;;
  ;; Check out `use-package!'s documentation for more about these two.
  (dolist (keyword '(:defer-incrementally :after-call))
    (push keyword use-package-deferring-keywords)
    (setq use-package-keywords
          (use-package-list-insert keyword use-package-keywords :after)))

  (defalias 'use-package-normalize/:defer-incrementally #'use-package-normalize-symlist)
  (defun use-package-handler/:defer-incrementally (name _keyword targets rest state)
    (use-package-concat
     `((doom-load-packages-incrementally
        ',(if (equal targets '(t))
              (list name)
            (append targets (list name)))))
     (use-package-process-keywords name rest state)))

  (defalias 'use-package-normalize/:after-call #'use-package-normalize-symlist)
  (defun use-package-handler/:after-call (name _keyword hooks rest state)
    (if (plist-get state :demand)
        (use-package-process-keywords name rest state)
      (let ((fn (make-symbol (format "doom--after-call-%s-h" name))))
        (use-package-concat
         `((fset ',fn
                 (lambda (&rest _)
                   (doom-log "Loading deferred package %s from %s" ',name ',fn)
                   (condition-case e
                       ;; If `default-directory' is a directory that doesn't
                       ;; exist or is unreadable, Emacs throws up file-missing
                       ;; errors, so we set it to a directory we know exists and
                       ;; is readable.
                       (let ((default-directory doom-emacs-dir))
                         (require ',name))
                     ((debug error)
                      (message "Failed to load deferred package %s: %s" ',name e)))
                   (when-let (deferral-list (assq ',name doom--deferred-packages-alist))
                     (dolist (hook (cdr deferral-list))
                       (advice-remove hook #',fn)
                       (remove-hook hook #',fn))
                     (delq! deferral-list doom--deferred-packages-alist)
                     (unintern ',fn nil)))))
         (let (forms)
           (dolist (hook hooks forms)
             (push (if (string-match-p "-\\(?:functions\\|hook\\)$" (symbol-name hook))
                       `(add-hook ',hook #',fn)
                     `(advice-add #',hook :before #',fn))
                   forms)))
         `((unless (assq ',name doom--deferred-packages-alist)
             (push '(,name) doom--deferred-packages-alist))
           (nconc (assq ',name doom--deferred-packages-alist)
                  '(,@hooks)))
         (use-package-process-keywords name rest state))))))


;;
;;; Module config macros

(put :if     'lisp-indent-function 2)
(put :when   'lisp-indent-function 'defun)
(put :unless 'lisp-indent-function 'defun)

(defmacro doom! (&rest modules)
  "Bootstraps DOOM Emacs and its modules.

If the first item in MODULES doesn't satisfy `keywordp', MODULES is evaluated,
otherwise, MODULES is a multiple-property list (a plist where each key can have
multiple, linear values).

The bootstrap process involves making sure the essential directories exist, core
packages are installed, `doom-autoload-file' is loaded, `doom-packages-file'
cache exists (and is loaded) and, finally, loads your private init.el (which
should contain your `doom!' block).

The overall load order of Doom is as follows:

  ~/.emacs.d/init.el
  ~/.emacs.d/core/core.el
  $DOOMDIR/init.el
  {$DOOMDIR,~/.emacs.d}/modules/*/*/init.el
  `doom-before-init-modules-hook'
  {$DOOMDIR,~/.emacs.d}/modules/*/*/config.el
  `doom-init-modules-hook'
  $DOOMDIR/config.el
  `doom-after-init-modules-hook'
  `after-init-hook'
  `emacs-startup-hook'
  `window-setup-hook'

Module load order is determined by your `doom!' block. See `doom-modules-dirs'
for a list of all recognized module trees. Order defines precedence (from most
to least)."
  `(let ((modules
          ,@(if (keywordp (car modules))
               (list (list 'quote modules))
             modules)))
     (unless doom-modules
       (setq doom-modules
             (make-hash-table :test 'equal
                              :size (if modules (length modules) 150)
                              :rehash-threshold 1.0)))
     (let ((inhibit-message doom-inhibit-module-warnings)
           obsolete category m)
       (while modules
         (setq m (pop modules))
         (cond ((keywordp m)
                (setq category m
                      obsolete (assq m doom-obsolete-modules)))
               ((not category)
                (error "No module category specified for %s" m))
               ((and (listp m) (keywordp (car m)))
                (pcase (car m)
                  (:cond
                   (cl-loop for (cond . mods) in (cdr m)
                            if (eval cond t)
                            return (prependq! modules mods)))
                  (:if (if (eval (cadr m) t)
                           (push (caddr m) modules)
                         (prependq! modules (cdddr m))))
                  (fn (if (or (eval (cadr m) t)
                              (eq fn :unless))
                          (prependq! modules (cddr m))))))
               ((catch 'doom-modules
                  (let* ((module (if (listp m) (car m) m))
                         (flags  (if (listp m) (cdr m))))
                    (when-let (new (assq module obsolete))
                      (let ((newkeys (cdr new)))
                        (if (null newkeys)
                            (message "WARNING %s module was removed" key)
                          (if (cdr newkeys)
                              (message "WARNING %s module was removed and split into the %s modules"
                                       (list category module) (mapconcat #'prin1-to-string newkeys ", "))
                            (message "WARNING %s module was moved to %s"
                                     (list category module) (car newkeys)))
                          (push category modules)
                          (dolist (key newkeys)
                            (push (if flags
                                      (nconc (cdr key) flags)
                                    (cdr key))
                                  modules)
                            (push (car key) modules))
                          (throw 'doom-modules t))))
                    (if-let (path (doom-module-locate-path category module))
                        (doom-module-set category module :flags flags :path path)
                      (message "WARNING Couldn't find the %s %s module" category module)))))))
       (unless doom-interactive-mode
         (setq doom-inhibit-module-warnings t))
       doom-modules)))

(defvar doom-disabled-packages)
(defmacro use-package! (name &rest plist)
  "Declares and configures a package.

This is a thin wrapper around `use-package', and is ignored if the NAME package
is disabled by the user (with `package!').

See `use-package' to see what properties can be provided. Doom adds support for
two extra properties:

:after-call SYMBOL|LIST
  Takes a symbol or list of symbols representing functions or hook variables.
  The first time any of these functions or hooks are executed, the package is
  loaded.

:defer-incrementally SYMBOL|LIST|t
  Takes a symbol or list of symbols representing packages that will be loaded
  incrementally at startup before this one. This is helpful for large packages
  like magit or org, which load a lot of dependencies on first load. This lets
  you load them piece-meal during idle periods, so that when you finally do need
  the package, it'll load quicker.

  NAME is implicitly added if this property is present and non-nil. No need to
  specify it. A value of `t' implies NAME."
  (declare (indent 1))
  (unless (or (memq name doom-disabled-packages)
              ;; At compile-time, use-package will forcibly load packages to
              ;; prevent compile-time errors. However, if a Doom user has
              ;; disabled packages you get file-missing package errors, so it's
              ;; necessary to check for packages at compile time:
              (and (bound-and-true-p byte-compile-current-file)
                   (not (locate-library (symbol-name name)))))
    `(use-package ,name ,@plist)))

(defmacro use-package-hook! (package when &rest body)
  "Reconfigures a package's `use-package!' block.

This macro must be used *before* PACKAGE's `use-package!' block. Often, this
means using it from your DOOMDIR/init.el.

Under the hood, this uses use-package's `use-package-inject-hooks'.

PACKAGE is a symbol; the package's name.
WHEN should be one of the following:
  :pre-init :post-init :pre-config :post-config

WARNINGS:
- The use of this macro is more often than not a code smell. Use it as last
  resort. There is almost always a better alternative.
- If you are using this solely for :post-config, stop! `after!' is much better.
- If :pre-init or :pre-config hooks return nil, the original `use-package!''s
  :init/:config block (respectively) is overwritten, so remember to have them
  return non-nil (or exploit that to overwrite Doom's config)."
  (declare (indent defun))
  (unless (memq when '(:pre-init :post-init :pre-config :post-config))
    (error "'%s' isn't a valid hook for use-package-hook!" when))
  `(progn
     (setq use-package-inject-hooks t)
     (add-hook ',(intern (format "use-package--%s--%s-hook"
                                 package
                                 (substring (symbol-name when) 1)))
               (lambda () ,@body)
               'append)))

(defmacro require! (category module &rest flags)
  "Loads the CATEGORY MODULE module with FLAGS.

CATEGORY is a keyword, MODULE is a symbol and FLAGS are symbols.

  (require! :lang php +lsp)

This is for testing and internal use. This is not the correct way to enable a
module."
  `(let ((doom-modules (or ,doom-modules (doom-modules)))
         (module-path (doom-module-locate-path ,category ',module)))
     (doom-module-set
      ,category ',module
      (let ((plist (doom-module-get ,category ',module)))
        ,(when flags
           `(plist-put plist :flags `,flags))
        (unless (plist-member plist :path)
          (plist-put plist :path ,(doom-module-locate-path category module)))
        plist))
     (if (directory-name-p module-path)
         (condition-case-unless-debug ex
             (let ((doom--current-module ',(cons category module))
                   (doom--current-flags ',flags))
               (load! "init" module-path :noerror)
               (load! "config" module-path :noerror))
           ('error
            (lwarn 'doom-modules :error
                   "%s in '%s %s' -> %s"
                   (car ex) ,category ',module
                   (error-message-string ex))))
       (warn 'doom-modules :warning "Couldn't find module '%s %s'"
             ,category ',module))))

(defmacro featurep! (category &optional module flag)
  "Returns t if CATEGORY MODULE is enabled.

If FLAG is provided, returns t if CATEGORY MODULE has FLAG enabled.

  (featurep! :config default)

Module FLAGs are set in your config's `doom!' block, typically in
~/.emacs.d/init.el. Like so:

  :config (default +flag1 -flag2)

CATEGORY and MODULE can be omitted When this macro is used from inside a module
(except your DOOMDIR, which is a special module). e.g. (featurep! +flag)"
  (and (cond (flag (memq flag (doom-module-get category module :flags)))
             (module (doom-module-p category module))
             (doom--current-flags (memq category doom--current-flags))
             ((let ((module (doom-module-from-path)))
                (unless module
                  (error "(featurep! %s %s %s) couldn't figure out what module it was called from (in %s)"
                         category module flag (file!)))
                (memq category (doom-module-get (car module) (cdr module) :flags)))))
       t))

;; DEPRECATED
(defmacro def-package! (&rest args)
  "Do not use this macro. Use `use-package!' instead."
  (warn "`def-package!' is deprecated and was renamed to `use-package!'")
  `(use-package! ,@args))
(make-obsolete 'def-package! 'use-package! "2.0.9")

(defmacro def-package-hook! (&rest args)
  "Do not use this macro. Use `use-package!' instead."
  (warn "`def-package-hook!' is deprecated and was renamed to `use-package-hook!'")
  `(use-package-hook! ,@args))
(make-obsolete 'def-package-hook! 'use-package-hook! "2.0.9")

(provide 'core-modules)
;;; core-modules.el ends here
