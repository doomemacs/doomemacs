;;; core-modules.el --- module & package management system -*- lexical-binding: t; -*-

(defvar doom-init-modules-p nil
  "Non-nil if `doom-initialize-modules' has run.")

(defvar doom-modules ()
  "A hash table of enabled modules. Set by `doom-initialize-modules'.")

(defvar doom-modules-dirs
  (list (expand-file-name "modules/" doom-private-dir)
        doom-modules-dir)
  "A list of module root directories. Order determines priority.")

(defvar doom-inhibit-module-warnings (not noninteractive)
  "If non-nil, don't emit deprecated or missing module warnings at startup.")

(defconst doom-obsolete-modules
  '((:feature (version-control (:emacs vc) (:ui vc-gutter))
              (spellcheck (:tools flyspell))
              (syntax-checker (:tools flycheck))
              (evil (:editor evil))
              (snippets (:editor snippets))
              (file-templates (:editor file-templates))
              (workspaces (:ui workspaces))
              (eval (:tools eval))
              (lookup (:tools lookup))
              (debugger (:tools debugger)))
    (:tools (rotate-text (:editor rotate-text))
            (vterm (:term vterm))
            (password-store (:tools pass)))
    (:emacs (electric-indent (:emacs electric))
            (hideshow (:editor fold))
            (eshell (:term eshell))
            (term (:term term)))
    (:ui (doom-modeline (:ui modeline))
         (fci (:ui fill-column))
         (evil-goggles (:ui ophints)))
    (:app (email (:email mu4e))
          (notmuch (:email notmuch))))
  "A tree alist that maps deprecated modules to their replacement(s).

Each entry is a three-level tree. For example:

  (:feature (version-control (:emacs vc) (:ui vc-gutter))
            (spellcheck (:tools flyspell))
            (syntax-checker (:tools flycheck)))

This marks :feature version-control, :feature spellcheck and :feature
syntax-checker modules obsolete. e.g. If :feature version-control is found in
your `doom!' block, a warning is emitted before replacing it with :emacs vc and
:ui vc-gutter.")

(defvar doom--current-module nil)
(defvar doom--current-flags nil)


;;
;;; Custom hooks

(defvar doom-before-init-modules-hook nil
  "A list of hooks to run before Doom's modules' config.el files are loaded, but
after their init.el files are loaded.")

(defvar doom-init-modules-hook nil
  "A list of hooks to run after Doom's modules' config.el files have loaded, but
before the user's private module.")

(defvaralias 'doom-after-init-modules-hook 'after-init-hook)

(define-obsolete-variable-alias 'doom-post-init-hook 'doom-init-modules-hook "2.1.0")
(define-obsolete-variable-alias 'doom-init-hook 'doom-before-init-modules-hook "2.1.0")


;;
;;; Bootstrap API

(defun doom-initialize-modules (&optional force-p)
  "Loads the init.el in `doom-private-dir' and sets up hooks for a healthy
session of Dooming. Will noop if used more than once, unless FORCE-P is
non-nil."
  (when (and (or force-p
                 (not doom-init-modules-p))
             (not (setq doom-modules nil))
             (load! "init" doom-private-dir t))
    (setq doom-init-modules-p t)
    (maphash (lambda (key plist)
               (let ((doom--current-module key)
                     (doom--current-flags (plist-get plist :flags)))
                 (load! "init" (plist-get plist :path) t)))
             doom-modules)
    (run-hook-wrapped 'doom-before-init-modules-hook #'doom-try-run-hook)
    (unless noninteractive
      (maphash (lambda (key plist)
                 (let ((doom--current-module key)
                       (doom--current-flags (plist-get plist :flags)))
                   (load! "config" (plist-get plist :path) t)))
               doom-modules)
      (run-hook-wrapped 'doom-init-modules-hook #'doom-try-run-hook)
      (load! "config" doom-private-dir t)
      (unless custom-file
        (setq custom-file (concat doom-local-dir "custom.el")))
      (when (stringp custom-file)
        (load custom-file t t t)))))


;;
;;; Module API

(defun doom-module-p (category module)
  "Returns t if CATEGORY MODULE is enabled (ie. present in `doom-modules')."
  (declare (pure t) (side-effect-free t))
  (and (hash-table-p doom-modules)
       (gethash (cons category module) doom-modules)
       t))

(defun doom-module-get (category module &optional property)
  "Returns the plist for CATEGORY MODULE. Gets PROPERTY, specifically, if set."
  (declare (pure t) (side-effect-free t))
  (when-let* ((plist (gethash (cons category module) doom-modules)))
    (if property
        (plist-get plist property)
      plist)))

(defun doom-module-put (category module &rest plist)
  "Set a PROPERTY for CATEGORY MODULE to VALUE. PLIST should be additional pairs
of PROPERTY and VALUEs.

\(fn CATEGORY MODULE PROPERTY VALUE &rest [PROPERTY VALUE [...]])"
  (if-let* ((old-plist (doom-module-get category module)))
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
  (let ((path (doom-module-get category module :path))
        file-name-handler-alist)
    (if file (expand-file-name file path)
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

(defun doom-module-from-path (&optional path)
  "Returns a cons cell (CATEGORY . MODULE) derived from PATH (a file path)."
  (or doom--current-module
      (let* (file-name-handler-alist
             (path (or path (FILE!))))
        (save-match-data
          (setq path (file-truename path))
          (when (string-match "/modules/\\([^/]+\\)/\\([^/]+\\)\\(?:/.*\\)?$" path)
            (when-let* ((category (match-string 1 path))
                        (module   (match-string 2 path)))
              (cons (doom-keyword-intern category)
                    (intern module))))))))

(defun doom-module-load-path (&optional all-p)
  "Return a list of absolute file paths to activated modules. If ALL-P is
non-nil, return paths of possible modules, activated or otherwise."
  (declare (pure t) (side-effect-free t))
  (append (if all-p
              (doom-files-in doom-modules-dirs
                             :type 'dirs
                             :mindepth 1
                             :depth 1
                             :full t)
            (cl-loop for plist being the hash-values of (doom-modules)
                     collect (plist-get plist :path)))
          (list doom-private-dir)))

(defun doom-modules (&optional refresh-p)
  "Minimally initialize `doom-modules' (a hash table) and return it."
  (or (unless refresh-p doom-modules)
      (let ((noninteractive t)
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
        use-package-expand-minimally (not noninteractive)))

;; Adds two new keywords to `use-package' (and consequently, `def-package!') to
;; expand its lazy-loading capabilities. They are:
;;
;; :after-call SYMBOL|LIST
;; :defer-incrementally SYMBOL|LIST|t
;;
;; Check out `def-package!'s documentation for more about these two.
(defvar doom--deferred-packages-alist '(t))

(with-eval-after-load 'use-package-core
  ;; Macros are already fontified, no need for this
  (font-lock-remove-keywords 'emacs-lisp-mode use-package-font-lock-keywords)

  ;; Disable :ensure and :pin, because they don't work with Doom because we do
  ;; our own package management.
  (with-eval-after-load 'use-package-ensure
    (dolist (keyword '(:ensure :pin))
      (delq! keyword use-package-keywords)
      (delq! keyword use-package-defaults 'assq)))

  ;; Insert new deferring keywords
  (dolist (keyword '(:defer-incrementally :after-call))
    (add-to-list 'use-package-deferring-keywords keyword nil #'eq)
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
      (let ((fn (make-symbol (format "doom|transient-hook--load-%s" name))))
        (use-package-concat
         `((fset ',fn
                 (lambda (&rest _)
                   (doom-log "Loading deferred package %s from %s" ',name ',fn)
                   (condition-case e
                       (require ',name)
                     ((debug error)
                      (message "Failed to load deferred package %s: %s" ',name e)))
                   (when-let* ((deferral-list (assq ',name doom--deferred-packages-alist)))
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

(defmacro doom! (&rest modules)
  "Bootstraps DOOM Emacs and its modules.

The bootstrap process involves making sure the essential directories exist, core
packages are installed, `doom-autoload-file' is loaded, `doom-packages-file'
cache exists (and is loaded) and, finally, loads your private init.el (which
should contain your `doom!' block).

If the cache exists, much of this function isn't run, which substantially
reduces startup time.

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
  (unless doom-modules
    (setq doom-modules
          (make-hash-table :test 'equal
                           :size (if modules (length modules) 150)
                           :rehash-threshold 1.0)))
  (let ((inhibit-message doom-inhibit-module-warnings)
        category m)
    (while modules
      (setq m (pop modules))
      (cond ((keywordp m) (setq category m))
            ((not category) (error "No module category specified for %s" m))
            ((catch 'doom-modules
               (let* ((module (if (listp m) (car m) m))
                      (flags  (if (listp m) (cdr m))))
                 (when-let* ((obsolete (assq category doom-obsolete-modules))
                             (new (assq module obsolete)))
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
                 (if-let* ((path (doom-module-locate-path category module)))
                     (doom-module-set category module :flags flags :path path)
                   (message "WARNING Couldn't find the %s %s module" category module))))))))
  (when noninteractive
    (setq doom-inhibit-module-warnings t))
  `(setq doom-modules ',doom-modules))

(defvar doom-disabled-packages)
(defmacro def-package! (name &rest plist)
  "Declares and configures a package.

This is a thin wrapper around `use-package', and is ignored if the NAME package
is disabled by the user (with `package!').

See `use-package' to see what properties can be provided. Doom adds support for
two extra properties:

:after-call SYMBOL|LIST
  Takes a symbol or list of symbols representing functions or hook variables.
  The first time any of these functions or hooks are executed, the package is
  loaded. e.g.

  (def-package! projectile
    :after-call (pre-command-hook after-find-file dired-before-readin-hook)
    ...)

:defer-incrementally SYMBOL|LIST|t
  Takes a symbol or list of symbols representing packages that will be loaded
  incrementally at startup before this one. This is helpful for large packages
  like magit or org, which load a lot of dependencies on first load. This lets
  you load them piece-meal during idle periods, so that when you finally do need
  the package, it'll load quicker. e.g.

  NAME is implicitly added if this property is present and non-nil. No need to
  specify it. A value of `t' implies NAME, e.g.

  (def-package! abc
    ;; This is equivalent to :defer-incrementally (abc)
    :defer-incrementally t
    ...)"
  (unless (or (memq name doom-disabled-packages)
              ;; At compile-time, use-package will forcibly load packages to
              ;; prevent compile-time errors. However, if a Doom user has
              ;; disabled packages you get file-missing package errors, so it's
              ;; necessary to check for packages at compile time:
              (and (bound-and-true-p byte-compile-current-file)
                   (not (locate-library (symbol-name name)))))
    `(use-package ,name ,@plist)))

(defmacro def-package-hook! (package when &rest body)
  "Reconfigures a package's `def-package!' block.

Only use this macro in a module's init.el file.

Under the hood, this uses use-package's `use-package-inject-hooks'.

PACKAGE is a symbol; the package's name.
WHEN should be one of the following:
  :pre-init :post-init :pre-config :post-config

WARNING: If :pre-init or :pre-config hooks return nil, the original
`def-package!''s :init/:config block (respectively) is overwritten, so remember
to have them return non-nil (or exploit that to overwrite Doom's config)."
  (declare (indent defun))
  (doom--assert-stage-p 'init #'package!)
  (unless (memq when '(:pre-init :post-init :pre-config :post-config))
    (error "'%s' isn't a valid hook for def-package-hook!" when))
  `(progn
     (setq use-package-inject-hooks t)
     (add-hook!
       ',(intern (format "use-package--%s--%s-hook"
                         package
                         (substring (symbol-name when) 1)))
       ,@body)))

(defmacro require! (category module &rest flags)
  "Loads the CATEGORY MODULE module with FLAGS.

CATEGORY is a keyword, MODULE is a symbol and FLAGS are symbols.

  (require! :lang php +lsp)

This is for testing and internal use. This is not the correct way to enable a
module."
  `(let ((doom-modules ,doom-modules)
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
               (let ((doom--stage 'config))
                 (load! "config" module-path :noerror)))
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
(except your DOOMDIR, which is a special moduel). e.g. (featurep! +flag)"
  (and (cond (flag (memq flag (doom-module-get category module :flags)))
             (module (doom-module-p category module))
             (doom--current-flags (memq category doom--current-flags))
             ((let ((module-pair
                     (or doom--current-module
                         (doom-module-from-path (FILE!)))))
                (unless module-pair
                  (error "featurep! call couldn't auto-detect what module its in (from %s)" (FILE!)))
                (memq category (doom-module-get (car module-pair) (cdr module-pair) :flags)))))
       t))

(defmacro after! (targets &rest body)
  "Evaluate BODY after TARGETS have loaded.

This is a wrapper around `with-eval-after-load' that:

1. Suppresses warnings for disabled packages at compile-time
2. No-ops for TARGETS that are disabled by the user (via `package!')
3. Supports compound TARGETS statements (see below)

TARGETS can either be:

- An unquoted package symbol (the name of a package)

    (after! helm BODY...)

- An unquoted list of package symbols (i.e. BODY is evaluated once both magit
  and git-gutter have loaded)

    (after! (magit git-gutter) BODY...)

- An unquoted, nested list of compound package lists, using :or/:any and/or :and/:all

    (after! (:or package-a package-b ...)  BODY...)
    (after! (:and package-a package-b ...) BODY...)
    (after! (:and package-a (:or package-b package-c) ...) BODY...)

Note that:
- :or and :any are equivalent
- :and and :all are equivalent
- If these are omitted, :and is implied."
  (declare (indent defun) (debug t))
  (unless (and (symbolp targets)
               (memq targets (bound-and-true-p doom-disabled-packages)))
    (list (if (or (not (bound-and-true-p byte-compile-current-file))
                  (dolist (next (doom-enlist targets))
                    (unless (keywordp next)
                      (if (symbolp next)
                          (require next nil :no-error)
                        (load next :no-message :no-error)))))
              #'progn
            #'with-no-warnings)
          (if (symbolp targets)
              `(with-eval-after-load ',targets ,@body)
            (pcase (car-safe targets)
              ((or :or :any)
               (macroexp-progn
                (cl-loop for next in (cdr targets)
                         collect `(after! ,next ,@body))))
              ((or :and :all)
               (dolist (next (cdr targets))
                 (setq body `((after! ,next ,@body))))
               (car body))
              (_ `(after! (:and ,@targets) ,@body)))))))

(provide 'core-modules)
;;; core-modules.el ends here
