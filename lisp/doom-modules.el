;;; doom-modules.el --- module & package management system -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;
;;; Variables

(defvar doom-modules (make-hash-table :test 'equal)
  "A hash table of enabled modules. Set by `doom-initialize-modules'.")

(defvar doom-modules-dirs
  (list (expand-file-name "modules/" doom-user-dir)
        doom-modules-dir)
  "A list of module root directories. Order determines priority.")

(defvar doom-module-init-file "init"
  "The basename of init files for modules.

Init files are loaded early, just after Doom core, and before modules' config
files. They are always loaded, even in non-interactive sessions, and before
`doom-before-modules-init-hook'. Related to `doom-module-config-file'.")

(defvar doom-module-config-file "config"
  "The basename of config files for modules.

Config files are loaded later, and almost always in interactive sessions. These
run before `doom-after-modules-config-hook'. Relevant to `doom-module-init-file'.")

(defconst doom-obsolete-modules
  '((:feature (version-control  (:emacs vc) (:ui vc-gutter))
              (spellcheck       (:checkers spell))
              (syntax-checker   (:checkers syntax))
              (evil             (:editor evil))
              (snippets         (:editor snippets))
              (file-templates   (:editor file-templates))
              (workspaces       (:ui workspaces))
              (eval             (:tools eval))
              (lookup           (:tools lookup))
              (debugger         (:tools debugger)))
    (:tools   (rotate-text      (:editor rotate-text))
              (vterm            (:term vterm))
              (password-store   (:tools pass))
              (flycheck         (:checkers syntax))
              (flyspell         (:checkers spell))
              (macos            (:os macos)))
    (:emacs   (electric-indent  (:emacs electric))
              (hideshow         (:editor fold))
              (eshell           (:term eshell))
              (term             (:term term)))
    (:ui      (doom-modeline    (:ui modeline))
              (fci              (:ui fill-column))
              (evil-goggles     (:ui ophints))
              (tabbar           (:ui tabs))
              (pretty-code      (:ui ligatures)))
    (:app     (email            (:email mu4e))
              (notmuch          (:email notmuch)))
    (:lang    (perl             (:lang raku))))
  "A tree alist that maps deprecated modules to their replacement(s).

Each entry is a three-level tree. For example:

  (:feature (version-control (:emacs vc) (:ui vc-gutter))
            (spellcheck (:checkers spell))
            (syntax-checker (:tools flycheck)))

This marks :feature version-control, :feature spellcheck and :feature
syntax-checker modules obsolete. e.g. If :feature version-control is found in
your `doom!' block, a warning is emitted before replacing it with :emacs vc and
:ui vc-gutter.")

(defvar doom-inhibit-module-warnings (not noninteractive)
  "If non-nil, don't emit deprecated or missing module warnings at startup.")

;;; Custom hooks
(defcustom doom-before-modules-init-hook nil
  "Hooks run before module init.el files are loaded."
  :group 'doom
  :type 'hook)

(defcustom doom-after-modules-init-hook nil
  "Hooks run after module init.el files are loaded."
  :group 'doom
  :type 'hook)

(defcustom doom-before-modules-config-hook nil
  "Hooks run before module config.el files are loaded."
  :group 'doom
  :type 'hook)

(defcustom doom-after-modules-config-hook nil
  "Hooks run after module config.el files are loaded (but before the user's)."
  :group 'doom
  :type 'hook)

(defvar doom--current-module nil)
(defvar doom--current-flags nil)


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
  (puthash (cons category module)
           (if-let (old-plist (doom-module-get category module))
               (if (null plist)
                   old-plist
                 (when (cl-oddp (length plist))
                   (signal 'wrong-number-of-arguments (list (length plist))))
                 (while plist
                   (plist-put old-plist (pop plist) (pop plist)))
                 old-plist)
             plist)
           doom-modules))

(defun doom-module-set (category module &rest plist)
  "Enables a module by adding it to `doom-modules'.

CATEGORY is a keyword, module is a symbol, PLIST is a plist that accepts the
following properties:

  :flags [SYMBOL LIST]  list of enabled category flags
  :path  [STRING]       path to category root directory

If PLIST consists of a single nil, unset and disable CATEGORY MODULE.

Example:
  (doom-module-set :lang 'haskell :flags '(+lsp))"
  (if (car plist)
      (progn
        ;; Doom caches flags and features using symbol plists for fast lookups in
        ;; `modulep!'. plists lack the overhead, and are much faster for datasets this
        ;; small. The format of this case is (cons FEATURES FLAGS)
        (put category module (cons t (plist-get plist :flags)))
        ;; But the hash table will always been Doom's formal storage for modules.
        (puthash (cons category module) plist doom-modules))
    (remhash (cons category module) doom-modules)
    (cl-remf (symbol-plist category) module)))

(defun doom-module-expand-path (category module &optional file)
  "Expands a path to FILE relative to CATEGORY and MODULE.

CATEGORY is a keyword. MODULE is a symbol. FILE is an optional string path.
If the category isn't enabled this returns nil. For finding disabled modules use
`doom-module-locate-path'."
  (when-let (path (doom-module-get category module :path))
    (if file
        (file-name-concat path file)
      path)))

(defun doom-module-locate-path (category &optional module file)
  "Searches `doom-modules-dirs' to find the path to a module.

CATEGORY is a keyword (e.g. :lang) and MODULE is a symbol (e.g. 'python). FILE
is a string that will be appended to the resulting path. If no path exists, this
returns nil, otherwise an absolute path."
  (let (file-name-handler-alist)
    (if-let (path (doom-module-expand-path category module file))
        (if (or (null file)
                (file-exists-p path))
            path)
      (let* ((category (doom-keyword-name category))
             (module (if module (symbol-name module)))
             (path (file-name-concat category module file)))
        (if file
            ;; PERF: locate-file-internal is a little faster for finding files,
            ;;   but its interface for finding directories is clumsy.
            (locate-file-internal path doom-modules-dirs)
          (cl-loop for default-directory in doom-modules-dirs
                   if (file-exists-p path)
                   return (expand-file-name path)))))))

(defun doom-module-from-path (path &optional enabled-only)
  "Returns a cons cell (CATEGORY . MODULE) derived from PATH (a file path).
If ENABLED-ONLY, return nil if the containing module isn't enabled."
  (let* ((file-name-handler-alist nil)
         (path (expand-file-name path)))
    (save-match-data
      (cond ((string-match "/modules/\\([^/]+\\)/\\([^/]+\\)\\(?:/.*\\)?$" path)
             (when-let* ((category (doom-keyword-intern (match-string 1 path)))
                         (module   (intern (match-string 2 path))))
               (and (or (null enabled-only)
                        (doom-module-p category module))
                    (cons category module))))
            ((file-in-directory-p path doom-core-dir)
             (cons :core nil))
            ((file-in-directory-p path doom-user-dir)
             (cons :user nil))))))

(defun doom-module-load-path (&optional module-dirs)
  "Return a list of file paths to activated modules.

The list is in no particular order and its file paths are absolute. If
MODULE-DIRS is non-nil, include all modules (even disabled ones) available in
those directories."
  (declare (pure t) (side-effect-free t))
  (if module-dirs
      (mapcar (lambda (m) (doom-module-locate-path (car m) (cdr m)))
              (delete-dups
               (doom-files-in module-dirs
                              :map #'doom-module-from-path
                              :type 'dirs
                              :mindepth 1
                              :depth 1)))
    (delq
     nil (cl-loop for (cat . mod) in (cddr (doom-module-list))
                  collect (doom-module-get cat mod :path)))))

(defun doom-module-mplist-map (fn mplist)
  "Apply FN to each module in MPLIST."
  (let ((mplist (copy-sequence mplist))
        (inhibit-message doom-inhibit-module-warnings)
        obsolete
        results
        category m)
    (while mplist
      (setq m (pop mplist))
      (cond ((keywordp m)
             (setq category m
                   obsolete (assq m doom-obsolete-modules)))
            ((null category)
             (error "No module category specified for %s" m))
            ((and (listp m) (keywordp (car m)))
             (pcase (car m)
               (:cond
                (cl-loop for (cond . mods) in (cdr m)
                         if (eval cond t)
                         return (prependq! mplist mods)))
               (:if (if (eval (cadr m) t)
                        (push (caddr m) mplist)
                      (prependq! mplist (cdddr m))))
               (test (if (xor (eval (cadr m) t)
                              (eq test :unless))
                         (prependq! mplist (cddr m))))))
            ((catch 'doom-modules
               (let* ((module (if (listp m) (car m) m))
                      (flags  (if (listp m) (cdr m))))
                 (when-let (new (assq module obsolete))
                   (let ((newkeys (cdr new)))
                     (if (null newkeys)
                         (print! (warn "%s module was removed"))
                       (if (cdr newkeys)
                           (print! (warn "%s module was removed and split into the %s modules")
                                   (list category module)
                                   (mapconcat #'prin1-to-string newkeys ", "))
                         (print! (warn "%s module was moved to %s")
                                 (list category module)
                                 (car newkeys)))
                       (push category mplist)
                       (dolist (key newkeys)
                         (push (if flags
                                   (nconc (cdr key) flags)
                                 (cdr key))
                               mplist)
                         (push (car key) mplist))
                       (throw 'doom-modules t))))
                 (push (funcall fn category module :flags (if (listp m) (cdr m)))
                       results))))))
    (when noninteractive
      (setq doom-inhibit-module-warnings t))
    (nreverse results)))

(defun doom-module-list (&optional all-p)
  "Return modules as a list of (:CATEGORY . MODULE) in their enabled order.

If ALL-P, return a list of *all* available modules instead, whether or not
they're enabled, and in lexicographical order.

If ALL-P is `real', only return *real"
  (if all-p
      (mapcar #'doom-module-from-path (doom-module-load-path doom-modules-dirs))
    (hash-table-keys doom-modules)))


;;
;;; Use-package modifications

(defvar doom--deferred-packages-alist '(t))

(autoload 'use-package "use-package-core" nil nil t)

(setq use-package-compute-statistics init-file-debug
      use-package-verbose init-file-debug
      use-package-minimum-reported-time (if init-file-debug 0 0.1)
      use-package-expand-minimally (not noninteractive))

;; A common mistake for new users is that they inadvertently install their
;; packages with package.el, by copying over old `use-package' declarations with
;; an :ensure t property. Doom doesn't use package.el, so this will throw an
;; error that will confuse beginners, so we disable `:ensure'.
(setq use-package-ensure-function
      (lambda (name &rest _)
        (message "Ignoring ':ensure t' in '%s' config" name)))
;; ...On the other hand, if the user has loaded `package', then we should assume
;; they know what they're doing and restore the old behavior:
(add-transient-hook! 'package-initialize
  (when (eq use-package-ensure-function #'ignore)
    (setq use-package-ensure-function #'use-package-ensure-elpa)))

(with-eval-after-load 'use-package-core
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

  ;; HACK Fix `:load-path' so it resolves relative paths to the containing file,
  ;;      rather than `user-emacs-directory'. This is a done as a convenience
  ;;      for users, wanting to specify a local directory.
  (defadvice! doom--resolve-load-path-from-containg-file-a (fn label arg &optional recursed)
    "Resolve :load-path from the current directory."
    :around #'use-package-normalize-paths
    ;; `use-package-normalize-paths' resolves paths relative to
    ;; `user-emacs-directory', so we change that.
    (let ((user-emacs-directory
           (or (and (stringp arg)
                    (not (file-name-absolute-p arg))
                    (ignore-errors (dir!)))
               doom-emacs-dir)))
      (funcall fn label arg recursed)))

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
                   (doom-log "use-package: lazy loading %s from %s" ',name ',fn)
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
packages are installed, `doom-autoloads-file' is loaded, `doom-packages-file'
cache exists (and is loaded) and, finally, loads your private init.el (which
should contain your `doom!' block).

Module load order is determined by your `doom!' block. See `doom-modules-dirs'
for a list of all recognized module trees. Order defines precedence (from most
to least)."
  `(when noninteractive
     (doom-module-mplist-map
      (lambda (category module &rest plist)
        (let ((path (doom-module-locate-path category module)))
          (unless path
            (print! (warn "Failed to locate a '%s %s' module") category module))
          (apply #'doom-module-set category module
                 :path path
                 plist)))
      ,@(if (keywordp (car modules))
            (list (list 'quote modules))
          modules))
     doom-modules))

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

;; DEPRECATED Remove in 3.0
(define-obsolete-function-alias 'featurep! 'modulep! "3.0.0")

(defmacro modulep! (category &optional module flag)
  "Return t if :CATEGORY MODULE (and +FLAGS) are enabled.

If FLAG is provided, returns t if CATEGORY MODULE has FLAG enabled.

  (modulep! :config default +flag)

CATEGORY and MODULE may be omitted when this macro is used from a Doom module's
source (except your DOOMDIR, which is a special module). Like so:

  (modulep! +flag)

For more about modules and flags, see `doom!'."
  (and (cond (flag (memq flag (cdr (get category module))))
             (module (get category module))
             (doom--current-flags (memq category doom--current-flags))
             (doom--current-module
              (memq category (cdr (get (car doom--current-module)
                                       (cdr doom--current-module)))))
             ((if-let (module (doom-module-from-path (macroexpand '(file!))))
                  (memq category (cdr (get (car module) (cdr module))))
                (error "(modulep! %s %s %s) couldn't figure out what module it was called from (in %s)"
                       category module flag (file!)))))
       t))


;;
;;; Defaults

;; Register Doom's two virtual module categories, representing Doom's core and
;; the user's config; which are always enabled.
(doom-module-set :core nil :path doom-core-dir)
(doom-module-set :user nil :path doom-user-dir)

(provide 'doom-modules)
;;; doom-modules.el ends here
