;;; core/core-packages.el -*- lexical-binding: t; -*-

;; Emacs package management is opinionated, and so is Doom. Doom uses `straight'
;; to create a declarative, lazy-loaded and (nominally) reproducible package
;; management system. We use `straight' over `package' because the latter is
;; tempermental. ELPA sources suffer downtime occasionally and often fail to
;; build packages when GNU Tar is unavailable (e.g. MacOS users start with BSD
;; tar). Known gnutls errors plague the current stable release of Emacs (26.x)
;; which bork TLS handshakes with ELPA repos (mainly gnu.elpa.org). See
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=3434.
;;
;; What's worse, you can only get the latest version of packages through ELPA.
;; In an ecosystem that is constantly changing, this is more frustrating than
;; convenient. Straight (and Doom) can do rolling release, but it is opt-in.
;;
;; Interacting with this package management system is done through Doom's
;; bin/doom script. Find out more about it by running 'doom help' (I highly
;; recommend you add the script to your PATH). Here are some highlights:
;;
;; + `bin/doom install`: a wizard that guides you through setting up Doom and
;;   your private config for the first time.
;; + `bin/doom sync`: your go-to command for making sure Doom is in optimal
;;   condition. It ensures all unneeded packages are removed, all needed ones
;;   are installed, and all metadata associated with them is generated.
;; + `bin/doom upgrade`: upgrades Doom Emacs and your packages to the latest
;;   versions. There's also 'bin/doom update' for updating only your packages.
;;
;; How this works is: the system reads packages.el files located in each
;; activated module, your private directory (`doom-private-dir'), and one in
;; `doom-core-dir'. These contain `package!' declarations that tell DOOM what
;; plugins to install and where from.
;;
;; All that said, you can still use package.el's commands, but 'bin/doom sync'
;; will purge ELPA packages.

(defvar doom-packages ()
  "A list of enabled packages. Each element is a sublist, whose CAR is the
package's name as a symbol, and whose CDR is the plist supplied to its
`package!' declaration. Set by `doom-initialize-packages'.")

(defvar doom-disabled-packages ()
  "A list of packages that should be ignored by `use-package!' and `after!'.")

(defvar doom-packages-file "packages"
  "The basename of packages file for modules.

Package files are read whenever Doom's package manager wants a manifest of all
desired packages. They are rarely read in interactive sessions (unless the user
uses a straight or package.el command directly).")


;;
;;; package.el

;; Ensure that, if we do need package.el, it is configured correctly. You really
;; shouldn't be using it, but it may be convenient for quick package testing.
(setq package-enable-at-startup nil
      package-user-dir (concat doom-local-dir "elpa/")
      package-gnupghome-dir (expand-file-name "gpg" package-user-dir)
      ;; I omit Marmalade because its packages are manually submitted rather
      ;; than pulled, so packages are often out of date with upstream.
      package-archives
      (let ((proto (if gnutls-verify-error "https" "http")))
        (list (cons "gnu"   (concat proto "://elpa.gnu.org/packages/"))
              (cons "melpa" (concat proto "://melpa.org/packages/"))
              (cons "org"   (concat proto "://orgmode.org/elpa/")))))

;; package.el has no business modifying the user's init.el
(advice-add #'package--ensure-init-file :override #'ignore)

;; Refresh package.el the first time you call `package-install', so it can still
;; be used (e.g. to temporarily test packages). Remember to run 'doom sync' to
;; purge them; they can conflict with packages installed via straight!
(add-transient-hook! 'package-install (package-refresh-contents))


;;
;;; Straight

(setq straight-base-dir doom-local-dir
      straight-repository-branch "develop"
      straight-cache-autoloads nil ; we already do this, and better.
      ;; Doom doesn't encourage you to modify packages in place. Disabling this
      ;; makes 'doom sync' instant (once everything set up), which is much nicer
      ;; UX than the several seconds modification checks.
      straight-check-for-modifications nil
      ;; We handle package.el ourselves (and a little more comprehensively)
      straight-enable-package-integration nil
      ;; Before switching to straight, `doom-local-dir' would average out at
      ;; around 100mb with half Doom's modules at ~230 packages. Afterwards, at
      ;; around 1gb. With shallow cloning, that is reduced to ~400mb. This has
      ;; no affect on packages that are pinned, however (run 'doom purge' to
      ;; compact those after-the-fact). Some packages break when shallow cloned
      ;; (like magit and org), but we'll deal with that elsewhere.
      straight-vc-git-default-clone-depth 1
      ;; Prefix declarations are unneeded bulk added to our autoloads file. Best
      ;; we don't have to deal with them at all.
      autoload-compute-prefixes nil
      ;; We handle it ourselves
      straight-fix-org nil
      ;; HACK Disable native-compilation for some troublesome files
      comp-deferred-compilation-black-list '("/evil-collection-vterm\\.el$"))

(defadvice! doom--read-pinned-packages-a (orig-fn &rest args)
  "Read `:pin's in `doom-packages' on top of straight's lockfiles."
  :around #'straight--lockfile-read-all
  (append (apply orig-fn args) ; lockfiles still take priority
          (doom-package-pinned-list)))


;;
;;; Bootstrappers

(defun doom--ensure-straight (recipe pin)
  (let ((repo-dir (doom-path straight-base-dir "straight/repos/straight.el"))
        (repo-url (concat "http" (if gnutls-verify-error "s")
                          "://github.com/"
                          (or (plist-get recipe :repo) "raxod502/straight.el")))
        (branch (or (plist-get recipe :branch) straight-repository-branch))
        (call (if doom-debug-p #'doom-exec-process #'doom-call-process)))
    (unless (file-directory-p repo-dir)
      (message "Installing straight...")
      (cond
       ((eq straight-vc-git-default-clone-depth 'full)
        (funcall call "git" "clone" "--origin" "origin" repo-url repo-dir))
       ((null pin)
        (funcall call "git" "clone" "--origin" "origin" repo-url repo-dir
                 "--depth" (number-to-string straight-vc-git-default-clone-depth)
                 "--branch" straight-repository-branch
                 "--single-branch" "--no-tags"))
       ((integerp straight-vc-git-default-clone-depth)
        (make-directory repo-dir t)
        (let ((default-directory repo-dir))
          (funcall call "git" "init")
          (funcall call "git" "checkout" "-b" straight-repository-branch)
          (funcall call "git" "remote" "add" "origin" repo-url)
          (funcall call "git" "fetch" "origin" pin
                   "--depth" (number-to-string straight-vc-git-default-clone-depth)
                   "--no-tags")
          (funcall call "git" "checkout" "--detach" pin)))))
    (require 'straight (concat repo-dir "/straight.el"))
    (doom-log "Initializing recipes")
    (with-temp-buffer
      (insert-file-contents (doom-path repo-dir "bootstrap.el"))
      ;; Don't install straight for us -- we've already done that -- only set
      ;; up its recipe repos for us.
      (eval-region (search-forward "(require 'straight)")
                   (point-max)))))

(defun doom--ensure-core-packages (packages)
  (doom-log "Installing core packages")
  (dolist (package packages)
    (let ((name (car package)))
      (when-let (recipe (plist-get (cdr package) :recipe))
        (straight-override-recipe (cons name recipe)))
      (straight-use-package name))))

(defun doom-initialize-core-packages (&optional force-p)
  "Ensure `straight' is installed and was compiled with this version of Emacs."
  (when (or force-p (null (bound-and-true-p straight-recipe-repositories)))
    (doom-log "Initializing straight")
    (let ((packages (doom-package-list nil 'core)))
      (cl-destructuring-bind (&key recipe pin &allow-other-keys)
          (alist-get 'straight packages)
        (doom--ensure-straight recipe pin))
      (doom--ensure-core-packages packages))))

(defun doom-initialize-packages (&optional force-p)
  "Process all packages, essential and otherwise, if they haven't already been.

If FORCE-P is non-nil, do it anyway.

This ensures `doom-packages' is populated and `straight' recipes are properly
processed."
  (doom-initialize-core-packages force-p)
  (when (or force-p (not (bound-and-true-p package--initialized)))
    (doom-log "Initializing package.el")
    (require 'package)
    (package-initialize)
    (unless package--initialized
      (error "Failed to initialize package.el")))
  (when (or force-p (null doom-packages))
    (doom-log "Initializing straight.el")
    (or (setq doom-disabled-packages nil
              doom-packages (doom-package-list))
        (error "Failed to read any packages"))
    (dolist (package doom-packages)
      (cl-destructuring-bind
          (name &key recipe disable ignore &allow-other-keys) package
        (unless ignore
          (if disable
              (cl-pushnew name doom-disabled-packages)
            (when recipe
              (straight-override-recipe (cons name recipe)))
            (straight-register-package name)))))))


;;
;;; Package management API

(defun doom-package-get (package &optional prop nil-value)
  "Returns PACKAGE's `package!' recipe from `doom-packages'."
  (let ((plist (cdr (assq package doom-packages))))
    (if prop
        (if (plist-member plist prop)
            (plist-get plist prop)
          nil-value)
      plist)))

(defun doom-package-set (package prop value)
  "Set PROPERTY in PACKAGE's recipe to VALUE."
  (setf (alist-get package doom-packages)
        (plist-put (alist-get package doom-packages)
                   prop value)))

(defun doom-package-recipe (package &optional prop nil-value)
  "Returns the `straight' recipe PACKAGE was registered with."
  (let ((plist (gethash (symbol-name package) straight--recipe-cache)))
    (if prop
        (if (plist-member plist prop)
            (plist-get plist prop)
          nil-value)
      plist)))

(defun doom-package-recipe-repo (package)
  "Resolve and return PACKAGE's (symbol) local-repo property."
  (if-let* ((recipe (cdr (straight-recipes-retrieve package)))
            (repo (straight-vc-local-repo-name recipe)))
      repo
    (symbol-name package)))

(defun doom-package-build-recipe (package &optional prop nil-value)
  "Returns the `straight' recipe PACKAGE was installed with."
  (let ((plist (nth 2 (gethash (symbol-name package) straight--build-cache))))
    (if prop
        (if (plist-member plist prop)
            (plist-get plist prop)
          nil-value)
      plist)))

(defun doom-package-dependencies (package &optional recursive _noerror)
  "Return a list of dependencies for a package."
  (let ((deps (nth 1 (gethash (symbol-name package) straight--build-cache))))
    (if recursive
        (append deps (mapcan (lambda (dep) (doom-package-dependencies dep t t))
                             (copy-sequence deps)))
      (copy-sequence deps))))

(defun doom-package-depending-on (package &optional noerror)
  "Return a list of packages that depend on the package named NAME."
  (cl-check-type name symbol)
  ;; can't get dependencies for built-in packages
  (unless (or (doom-package-build-recipe name)
              noerror)
    (error "Couldn't find %s, is it installed?" name))
  (cl-loop for pkg in (hash-table-keys straight--build-cache)
           for deps = (doom-package-dependencies pkg)
           if (memq package deps)
           collect pkg
           and append (doom-package-depending-on pkg t)))


;;; Predicate functions
(defun doom-package-built-in-p (package)
  "Return non-nil if PACKAGE (a symbol) is built-in."
  (eq (doom-package-build-recipe package :type)
      'built-in))

(defun doom-package-installed-p (package)
  "Return non-nil if PACKAGE (a symbol) is installed."
  (file-directory-p (straight--build-dir (symbol-name package))))

(defun doom-package-is-type-p (package type)
  "TODO"
  (memq type (doom-enlist (doom-package-get package :type))))

(defun doom-package-in-module-p (package category &optional module)
  "Return non-nil if PACKAGE was installed by the user's private config."
  (when-let (modules (doom-package-get package :modules))
    (or (and (not module) (assq :private modules))
        (member (cons category module) modules))))

(defun doom-package-backend (package)
  "Return 'straight, 'builtin, 'elpa or 'other, depending on how PACKAGE is
installed."
  (cond ((gethash (symbol-name package) straight--build-cache)
         'straight)
        ((or (doom-package-built-in-p package)
             (assq package package--builtins))
         'builtin)
        ((assq package package-alist)
         'elpa)
        ((locate-library (symbol-name package))
         'other)))


;;; Package getters
(defun doom--read-packages (file &optional noeval noerror)
  (condition-case-unless-debug e
      (with-temp-buffer ; prevent buffer-local state from propagating
        (if (not noeval)
            (load file noerror 'nomessage 'nosuffix)
          (when (file-exists-p file)
            (insert-file-contents file)
            (let (emacs-lisp-mode) (emacs-lisp-mode))
            ;; Scrape `package!' blocks from FILE for a comprehensive listing of
            ;; packages used by this module.
            (while (search-forward "(package!" nil t)
              (let ((ppss (save-excursion (syntax-ppss))))
                ;; Don't collect packages in comments or strings
                (unless (or (nth 3 ppss)
                            (nth 4 ppss))
                  (goto-char (match-beginning 0))
                  (cl-destructuring-bind (_ name . plist)
                      (read (current-buffer))
                    (push (cons
                           name (plist-put
                                 plist :modules
                                 (list (doom-module-from-path file))))
                          doom-packages))))))))
    (error
     (signal 'doom-package-error
             (list (doom-module-from-path file)
                   file e)))))

(defun doom-package-list (&optional all-p core-only-p)
  "Retrieve a list of explicitly declared packages from enabled modules.

If ALL-P, gather packages unconditionally across all modules, including disabled
ones."
  (let ((packages-file (concat doom-packages-file ".el"))
        doom-packages)
    (doom--read-packages
     (doom-path doom-core-dir packages-file) all-p 'noerror)
    (unless core-only-p
      (let ((private-packages (doom-path doom-private-dir packages-file))
            (doom-modules (doom-module-list)))
        (if all-p
            (mapc #'doom--read-packages
                  (doom-files-in doom-modules-dir
                                 :depth 2
                                 :match "/packages\\.el$"))
          ;; We load the private packages file twice to ensure disabled packages
          ;; are seen ASAP, and a second time to ensure privately overridden
          ;; packages are properly overwritten.
          (doom--read-packages private-packages nil 'noerror)
          (cl-loop for key being the hash-keys of doom-modules
                   for path = (doom-module-path (car key) (cdr key) packages-file)
                   for doom--current-module = key
                   do (doom--read-packages path nil 'noerror)))
        (doom--read-packages private-packages all-p 'noerror)))
    (cl-remove-if-not
     (if core-only-p
         (lambda (pkg) (eq (plist-get (cdr pkg) :type) 'core))
       #'identity)
     (nreverse doom-packages))))

(defun doom-package-pinned-list ()
  "Return an alist mapping package names (strings) to pinned commits (strings)."
  (let (alist)
    (dolist (package doom-packages alist)
      (cl-destructuring-bind (name &key disable ignore pin unpin &allow-other-keys)
          package
        (when (and (not ignore)
                   (not disable)
                   (or pin unpin))
          (setf (alist-get (doom-package-recipe-repo name) alist
                           nil 'remove #'equal)
                (unless unpin pin)))))))

(defun doom-package-recipe-list ()
  "Return straight recipes for non-builtin packages with a local-repo."
  (let (recipes)
    (dolist (recipe (hash-table-values straight--recipe-cache))
      (cl-destructuring-bind (&key local-repo type &allow-other-keys)
          recipe
        (unless (or (null local-repo)
                    (eq type 'built-in))
          (push recipe recipes))))
    (nreverse recipes)))


;;
;;; Module package macros

(cl-defmacro package!
    (name &rest plist &key built-in recipe ignore _type _pin _disable)
  "Declares a package and how to install it (if applicable).

This macro is declarative and does not load nor install packages. It is used to
populate `doom-packages' with metadata about the packages Doom needs to keep
track of.

Only use this macro in a module's packages.el file.

Accepts the following properties:

 :type core|local|built-in|virtual
   Specifies what kind of package this is. Can be a symbol or a list thereof.
     `core' = this is a protected package and cannot be disabled!
     `local' = this package is being modified in-place. This package's repo is
       unshallowed and will be skipped when you update packages.
     `built-in' = this package is already built-in (otherwise, will be
       installed)
     `virtual' = this package is not tracked by Doom's package manager. It won't
       be installed or uninstalled. Use this to pin 2nd order dependencies.
 :recipe RECIPE
   Specifies a straight.el recipe to allow you to acquire packages from external
   sources. See https://github.com/raxod502/straight.el#the-recipe-format for
   details on this recipe.
 :disable BOOL
   Do not install or update this package AND disable all of its `use-package!'
   and `after!' blocks.
 :ignore FORM
   Do not install this package.
 :pin STR|nil
   Pin this package to commit hash STR. Setting this to nil will unpin this
   package if previously pinned.
 :built-in BOOL|'prefer
   Same as :ignore if the package is a built-in Emacs package. This is more to
   inform help commands like `doom/help-packages' that this is a built-in
   package. If set to 'prefer, the package will not be installed if it is
   already provided by Emacs.

Returns t if package is successfully registered, and nil if it was disabled
elsewhere."
  (declare (indent defun))
  (when (and recipe (keywordp (car-safe recipe)))
    (plist-put! plist :recipe `(quote ,recipe)))
  ;; :built-in t is basically an alias for :ignore (locate-library NAME)
  (when built-in
    (when (and (not ignore)
               (equal built-in '(quote prefer)))
      (setq built-in `(locate-library ,(symbol-name name) nil doom--initial-load-path)))
    (plist-delete! plist :built-in)
    (plist-put! plist :ignore built-in))
  `(let* ((name ',name)
          (plist (cdr (assq name doom-packages))))
     ;; Record what module this declaration was found in
     (let ((module-list (plist-get plist :modules))
           (module ',(doom-module-from-path)))
       (unless (member module module-list)
         (plist-put! plist :modules
                     (append module-list
                             (list module)
                             (when (file-in-directory-p ,(dir!) doom-private-dir)
                               '((:private . modules)))
                             nil))))
     ;; Merge given plist with pre-existing one
     (doplist! ((prop val) (list ,@plist) plist)
       (unless (null val)
         (plist-put! plist prop val)))
     ;; Some basic key validation; throws an error on invalid properties
     (condition-case e
         (when-let (recipe (plist-get plist :recipe))
           (cl-destructuring-bind
               (&key local-repo _files _flavor
                     _no-build _no-byte-compile _no-autoloads
                     _type _repo _host _branch _remote _nonrecursive _fork _depth)
               recipe
             ;; Expand :local-repo from current directory
             (when local-repo
               (plist-put!
                plist :recipe
                (plist-put recipe :local-repo
                           (let ((local-path (expand-file-name local-repo ,(dir!))))
                             (if (file-directory-p local-path)
                                 local-path
                               local-repo)))))))
       (error
        (signal 'doom-package-error
                (cons ,(symbol-name name)
                      (error-message-string e)))))
     ;; This is the only side-effect of this macro!
     (setf (alist-get name doom-packages) plist)
     (unless (plist-get plist :disable)
       (with-no-warnings
         (cons name plist)))))

(defmacro disable-packages! (&rest packages)
  "A convenience macro for disabling packages in bulk.
Only use this macro in a module's (or your private) packages.el file."
  (macroexp-progn
   (mapcar (lambda (p) `(package! ,p :disable t))
           packages)))

(defmacro unpin! (&rest targets)
  "Unpin packages in TARGETS.

This unpins packages, so that 'doom upgrade' downloads their latest version. It
can be used one of five ways:

+ To disable pinning wholesale: (unpin! t)
+ To unpin individual packages: (unpin! packageA packageB ...)
+ To unpin all packages in a group of modules: (unpin! :lang :tools ...)
+ To unpin packages in individual modules:
    (unpin! (:lang python javascript) (:tools docker))

Or any combination of the above.

This macro should only be used from the user's private packages.el. No module
should use it!"
  (if (memq t targets)
      `(mapc (doom-rpartial #'doom-package-set :unpin t)
             (mapcar #'car doom-packages))
    (macroexp-progn
     (mapcar
      (lambda (target)
        (when target
          `(doom-package-set ',target :unpin t)))
      (cl-loop for target in targets
               if (or (keywordp target) (listp target))
               append
               (cl-loop with (category . modules) = (doom-enlist target)
                        for (name . plist) in doom-packages
                        for pkg-modules = (plist-get plist :modules)
                        if (and (assq category pkg-modules)
                                (or (null modules)
                                    (cl-loop for module in modules
                                             if (member (cons category module) pkg-modules)
                                             return t))
                                name)
                        collect it)
               else if (symbolp target)
               collect target)))))

(provide 'core-packages)
;;; core-packages.el ends here
