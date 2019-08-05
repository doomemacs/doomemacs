;;; core/autoload/packages.el -*- lexical-binding: t; -*-

(require 'core-packages)
(load! "cache") ; in case autoloads haven't been generated yet


(defun doom--packages-choose (prompt)
  (let ((table (cl-loop for pkg in package-alist
                        unless (doom-package-built-in-p (cdr pkg))
                        collect (cons (package-desc-full-name (cdr pkg))
                                      (cdr pkg)))))
    (cdr (assoc (completing-read prompt
                                 (mapcar #'car table)
                                 nil t)
                table))))

(defun doom--refresh-pkg-cache ()
  "Clear the cache for `doom-refresh-packages-maybe'."
  (setq doom--refreshed-p nil)
  (doom-cache-set 'last-pkg-refresh nil))

;;;###autoload
(defun doom-refresh-packages-maybe (&optional force-p)
  "Refresh ELPA packages, if it hasn't been refreshed recently."
  (when force-p
    (doom--refresh-pkg-cache))
  (unless (or (doom-cache-get 'last-pkg-refresh)
              doom--refreshed-p)
    (condition-case e
        (progn
          (message "Refreshing package archives")
          (package-refresh-contents)
          (doom-cache-set 'last-pkg-refresh t 1200))
    ((debug error)
     (doom--refresh-pkg-cache)
     (signal 'doom-error e)))))


;;
;;; Package metadata

;;;###autoload
(defun doom-package-plist (package)
  "Returns PACKAGE's `package!' recipe from `doom-packages'."
  (cdr (assq package doom-packages)))

;;;###autoload
(defun doom-package-desc (package)
  "Returns PACKAGE's desc struct from `package-alist'."
  (cadr (assq (or (car (doom-package-prop package :recipe))
                  package)
              package-alist)))

;;;###autoload
(defun doom-package-true-name (package)
  "Return PACKAGE's true name.

It is possible for quelpa packages to be given a psuedonym (the first argument
of `package!'). Its real name is the car of package's :recipe. e.g.

  (package! X :recipe (Y :fetcher github :repo \"abc/def\"))

X's real name is Y."
  (let ((sym (car (doom-package-prop package :recipe))))
    (or (and (symbolp sym)
             (not (keywordp sym))
             sym)
        package)))

;;;###autoload
(defun doom-package-psuedo-name (package)
  "TODO"
  (or (cl-loop for (package . plist) in doom-packages
               for recipe-name = (car (plist-get plist :recipe))
               if (eq recipe-name package)
               return recipe-name)
      package))

;;;###autoload
(defun doom-package-backend (package &optional noerror)
  "Return backend that PACKAGE was installed with.

Can either be elpa, quelpa or emacs (built-in). Throws an error if NOERROR is
nil and the package isn't installed.

See `doom-package-recipe-backend' to get the backend PACKAGE is registered with
\(as opposed to what it is was installed with)."
  (cl-check-type package symbol)
  (let ((package-truename (doom-package-true-name package)))
    (cond ((assq package-truename quelpa-cache)  'quelpa)
          ((assq package-truename package-alist) 'elpa)
          ((doom-package-built-in-p package) 'emacs)
          ((not noerror) (error "%s package is not installed" package)))))

;;;###autoload
(defun doom-package-recipe-backend (package &optional noerror)
  "Return backend that PACKAGE is registered with.

See `doom-package-backend' to get backend for currently installed package."
  (cl-check-type package symbol)
  (cond ((not (doom-package-registered-p package))
         (unless noerror
           (error "%s package is not registered" package)))
        ((let ((builtin (eval (doom-package-prop package :built-in) t)))
           (or (and (eq builtin 'prefer)
                    (locate-library (symbol-name package) nil doom-site-load-path))
               (eq builtin 't)))
         'emacs)
        ((doom-package-prop package :recipe)
         'quelpa)
        ('elpa)))

;;;###autoload
(defun doom-package-prop (package prop &optional nil-value)
  "Return PROPerty in PACKAGE's plist.

Otherwise returns NIL-VALUE if package isn't registered or PROP doesn't
exist/isn't specified."
  (cl-check-type package symbol)
  (cl-check-type prop keyword)
  (if-let (plist (doom-package-plist package))
      (if (plist-member plist prop)
          (plist-get plist prop)
        nil-value)
    nil-value))


;;
;;; Predicate functions

;;;###autoload
(defun doom-package-built-in-p (package)
  "Return non-nil if PACKAGE (a symbol) is built-in."
  (unless (doom-package-installed-p package)
    (or (package-built-in-p (doom-package-true-name package))
        (locate-library (symbol-name package) nil doom-site-load-path))))

;;;###autoload
(defun doom-package-installed-p (package)
  "Return non-nil if PACKAGE (a symbol) is installed."
  (when-let (desc (doom-package-desc package))
    (and (package-installed-p desc)
         (file-directory-p (package-desc-dir desc)))))

;;;###autoload
(defun doom-package-registered-p (package)
  "Return non-nil if PACKAGE (a symbol) has been registered with `package!'.

Excludes packages that have a non-nil :built-in property."
  (let ((package (or (cl-loop for (pkg . plist) in doom-packages
                              for newname = (car (plist-get plist :recipe))
                              if (and (symbolp newname)
                                      (eq newname package))
                              return pkg)
                     package)))
    (when-let (plist (doom-package-plist package))
      (not (eval (plist-get plist :ignore))))))

;;;###autoload
(defun doom-package-private-p (package)
  "Return non-nil if PACKAGE was installed by the user's private config."
  (doom-package-prop package :private))

;;;###autoload
(defun doom-package-protected-p (package)
  "Return non-nil if PACKAGE is protected.

A protected package cannot be deleted and will be auto-installed if missing."
  (memq (doom-package-true-name package) doom-core-packages))

;;;###autoload
(defun doom-package-core-p (package)
  "Return non-nil if PACKAGE is a core Doom package."
  (or (doom-package-protected-p package)
      (assq :core (doom-package-prop package :modules))))

;;;###autoload
(defun doom-package-different-backend-p (package)
  "Return t if a PACKAGE (a symbol) has a new backend than what it was installed
with. Returns nil otherwise, or if package isn't installed."
  (cl-check-type package symbol)
  (and (doom-package-installed-p package)
       (not (doom-get-depending-on package)) ; not a dependency
       (not (eq (doom-package-backend package 'noerror)
                (doom-package-recipe-backend package 'noerror)))))

;;;###autoload
(defun doom-package-different-recipe-p (name)
  "Return t if a package named NAME (a symbol) has a different recipe than it
was installed with."
  (cl-check-type name symbol)
  (when (doom-package-installed-p name)
    (let ((package-truename (doom-package-true-name name)))
      (when-let* ((quelpa-recipe (assq package-truename quelpa-cache))
                  (doom-recipe   (assq package-truename doom-packages)))
        (not (equal (cdr quelpa-recipe)
                    (cdr (plist-get (cdr doom-recipe) :recipe))))))))

(defvar quelpa-upgrade-p)
;;;###autoload
(defun doom-package-outdated-p (name)
  "Determine whether NAME (a symbol) is outdated or not.

If outdated, returns a list, whose car is NAME, and cdr the current version list
and latest version list of the package."
  (cl-check-type name symbol)
  (when-let (desc (doom-package-desc name))
    (let* ((old-version (package-desc-version desc))
           (new-version
            (pcase (doom-package-backend name)
              (`quelpa
               (let ((recipe (doom-package-prop name :recipe))
                     (dir (expand-file-name (symbol-name name) quelpa-build-dir))
                     (inhibit-message (not doom-debug-mode))
                     (quelpa-upgrade-p t))
                 (if-let (ver (quelpa-checkout recipe dir))
                     (version-to-list ver)
                   old-version)))
              (`elpa
               (let ((desc (cadr (assq name package-archive-contents))))
                 (when (package-desc-p desc)
                   (package-desc-version desc)))))))
      (unless (and (listp old-version) (listp new-version))
        (error "Couldn't get version for %s" name))
      (when (version-list-< old-version new-version)
        (list name old-version new-version)))))


;;
;;; Package list getters

;;;###autoload
(cl-defun doom-find-packages (&key (installed 'any)
                                   (private 'any)
                                   (disabled 'any)
                                   (pinned 'any)
                                   (ignored 'any)
                                   (core 'any)
                                   _changed
                                   backend
                                   deps)
  "Retrieves a list of primary packages (i.e. non-dependencies). Each element is
a cons cell, whose car is the package symbol and whose cdr is the quelpa recipe
(if any).

You can build a filtering criteria using one or more of the following
properties:

  :backend 'quelpa|'elpa|'emacs|'any
    Include packages installed through 'quelpa, 'elpa or 'emacs. 'any is the
    wildcard.
  :installed BOOL|'any
    t = only include installed packages
    nil = exclude installed packages
  :private BOOL|'any
    t = only include user-installed packages
    nil = exclude user-installed packages
  :core BOOL|'any
    t = only include Doom core packages
    nil = exclude Doom core packages
  :disabled BOOL|'any
    t = only include disabled packages
    nil = exclude disabled packages
  :ignored BOOL|'any
    t = only include ignored packages
    nil = exclude ignored packages
  :pinned BOOL|ARCHIVE
    Only return packages that are pinned (t), not pinned (nil) or pinned to a
    specific archive (stringp)
  :deps BOOL
    Includes the package's dependencies (t) or not (nil).

Warning: this function is expensive, as it re-evaluates your all packages.el
files."
  (delete-dups
   (cl-loop for (sym . plist) in doom-packages
            if (and (or (not backend)
                        (eq (doom-package-backend sym 'noerror) backend))
                    (or (eq ignored 'any)
                        (let* ((form (plist-get plist :ignore))
                               (value (eval form)))
                          (if ignored value (not value))))
                    (or (eq disabled 'any)
                        (if disabled
                            (plist-get plist :disable)
                          (not (plist-get plist :disable))))
                    (or (eq installed 'any)
                        (if installed
                            (doom-package-installed-p sym)
                          (not (doom-package-installed-p sym))))
                    (or (eq private 'any)
                        (let ((modules (plist-get plist :modules)))
                          (if private
                              (assq :private modules)
                            (not (assq :private modules)))))
                    (or (eq core 'any)
                        (let ((modules (plist-get plist :modules)))
                          (if core
                              (assq :core modules)
                            (not (assq :core modules)))))
                    (or (eq pinned 'any)
                        (cond ((eq pinned 't)
                               (plist-get plist :pin))
                              ((null pinned)
                               (not (plist-get plist :pin)))
                              ((equal (plist-get plist :pin) pinned)))))
            collect (cons sym plist)
            and if (and deps (not (doom-package-built-in-p sym)))
            nconc
            (cl-loop for pkg in (doom-get-dependencies-for sym 'recursive 'noerror)
                     if (or (eq installed 'any)
                            (if installed
                                (doom-package-installed-p pkg)
                              (not (doom-package-installed-p pkg))))
                     collect (cons pkg (cdr (assq pkg doom-packages)))))))

(defun doom--read-module-packages-file (file &optional raw noerror)
  (with-temp-buffer ; prevent buffer-local settings from propagating
    (condition-case e
        (if (not raw)
            (load file noerror t t)
          (when (file-readable-p file)
            (insert-file-contents file)
            (while (re-search-forward "(package! " nil t)
              (save-excursion
                (goto-char (match-beginning 0))
                (cl-destructuring-bind (name . plist) (cdr (sexp-at-point))
                  (push (cons name
                              (plist-put plist :modules
                                         (cond ((file-in-directory-p file doom-private-dir)
                                                '((:private)))
                                               ((file-in-directory-p file doom-core-dir)
                                                '((:core)))
                                               ((doom-module-from-path file)))))
                        doom-packages))))))
      ((debug error)
       (signal 'doom-package-error
               (list (or (doom-module-from-path file)
                         '(:private . packages))
                     e))))))

;;;###autoload
(defun doom-package-list (&optional all-p)
  "Retrieve a list of explicitly declared packages from enabled modules.

This excludes core packages listed in `doom-core-packages'.

If ALL-P, gather packages unconditionally across all modules, including disabled
ones."
  (let ((noninteractive t)
        (doom--stage 'packages)
        (doom-modules (doom-modules))
        doom-packages
        doom-disabled-packages
        package-pinned-packages)
    (doom--read-module-packages-file (expand-file-name "packages.el" doom-core-dir) all-p)
    (let ((private-packages (expand-file-name "packages.el" doom-private-dir)))
      (unless all-p
        ;; We load the private packages file twice to ensure disabled packages
        ;; are seen ASAP, and a second time to ensure privately overridden
        ;; packages are properly overwritten.
        (doom--read-module-packages-file private-packages nil t))
      (if all-p
          (mapc #'doom--read-module-packages-file
                (doom-files-in doom-modules-dir
                               :depth 2
                               :full t
                               :match "/packages\\.el$"
                               :sort nil))
        (cl-loop for key being the hash-keys of doom-modules
                 for path = (doom-module-path (car key) (cdr key) "packages.el")
                 for doom--current-module = key
                 do (doom--read-module-packages-file path nil t)))
      (doom--read-module-packages-file private-packages all-p t))
    (append (cl-loop for package in doom-core-packages
                     collect (list package :modules '((:core internal))))
            (nreverse doom-packages))))

;;;###autoload
(defun doom-get-package-alist ()
  "Returns a list of all desired packages, their dependencies and their desc
objects, in the order of their `package! blocks.'"
  (cl-remove-duplicates
   (cl-loop for name in (mapcar #'car doom-packages)
            if (assq name package-alist)
            nconc (cl-loop for dep in (package--get-deps name)
                           if (assq dep package-alist)
                           collect (cons dep (cadr it)))
            and collect (cons name (cadr it)))
   :key #'car
   :from-end t))

;;;###autoload
(defun doom-get-depending-on (name &optional noerror)
  "Return a list of packages that depend on the package named NAME."
  (cl-check-type name symbol)
  (setq name (or (car (doom-package-prop name :recipe)) name))
  (unless (doom-package-built-in-p name)
    (if-let (desc (cadr (assq name package-alist)))
        (mapcar #'package-desc-name (package--used-elsewhere-p desc nil t))
      (unless noerror
        (error "Couldn't find %s, is it installed?" name)))))

;;;###autoload
(defun doom-get-dependencies-for (name &optional recursive noerror)
  "Return a list of dependencies for a package."
  (cl-check-type name symbol)
  ;; can't get dependencies for built-in packages
  (unless (doom-package-built-in-p name)
    (if-let (desc (doom-package-desc name))
        (let* ((deps (mapcar #'car (package-desc-reqs desc)))
               (deps (cl-remove-if #'doom-package-built-in-p deps)))
          (if recursive
              (nconc deps (mapcan (lambda (dep) (doom-get-dependencies-for dep t t))
                                  deps))
            deps))
      (unless noerror
        (error "Couldn't find %s, is it installed?" name)))))

;;;###autoload
(defun doom-get-outdated-packages (&optional include-frozen-p)
  "Return a list of packages that are out of date. Each element is a list,
containing (PACKAGE-SYMBOL OLD-VERSION-LIST NEW-VERSION-LIST).

If INCLUDE-FROZEN-P is non-nil, check frozen packages as well.

Used by `doom-packages-update'."
  (doom-refresh-packages-maybe doom-debug-mode)
  (cl-loop for package in (mapcar #'car package-alist)
           when (and (or (not (eval (doom-package-prop package :freeze)))
                         include-frozen-p)
                     (not (eval (doom-package-prop package :ignore)))
                     (not (doom-package-different-backend-p package))
                     (doom-package-outdated-p package))
           collect it))

;;;###autoload
(defun doom-get-orphaned-packages ()
  "Return a list of symbols representing packages that are no longer needed or
depended on.

Used by `doom-packages-autoremove'."
  (let ((package-selected-packages
         (mapcar #'car (doom-find-packages :ignored nil :disabled nil))))
    (append (cl-remove-if #'doom-package-registered-p (package--removable-packages))
            (cl-loop for pkg in package-selected-packages
                     if (and (doom-package-different-backend-p pkg)
                             (not (doom-package-built-in-p pkg)))
                     collect pkg))))

;;;###autoload
(defun doom-get-missing-packages ()
  "Return a list of requested packages that aren't installed or built-in, but
are enabled (with a `package!' directive). Each element is a list whose CAR is
the package symbol, and whose CDR is a plist taken from that package's
`package!' declaration.

Used by `doom-packages-install'."
  (cl-loop for (name . plist)
           in (doom-find-packages :ignored nil
                                  :disabled nil
                                  :deps t)
           if (and (equal (plist-get plist :pin)
                          (ignore-errors
                            (package-desc-archive
                             (cadr (assq name package-alist)))))
                   (or (not (doom-package-installed-p name))
                       (doom-package-different-backend-p name)
                       (doom-package-different-recipe-p name)))
           collect (cons name plist)))


;;
;; Main functions

(defun doom--delete-package-files (name-or-desc)
  (let ((pkg-build-dir
         (if (package-desc-p name-or-desc)
             (package-desc-dir name-or-desc)
           (expand-file-name (symbol-name name-or-desc) quelpa-build-dir))))
    (when (file-directory-p pkg-build-dir)
      (delete-directory pkg-build-dir t))))

;;;###autoload
(defun doom-install-package (name &optional plist)
  "Installs package NAME with optional quelpa RECIPE (see `quelpa-recipe' for an
example; the package name can be omitted)."
  (cl-check-type name symbol)
  (when (and (doom-package-installed-p name)
             (not (doom-package-built-in-p name)))
    (if (or (doom-package-different-backend-p name)
            (doom-package-different-recipe-p name))
        (doom-delete-package name t)
      (user-error "%s is already installed" name)))
  (let* ((inhibit-message (not doom-debug-mode))
         (plist (or plist (doom-package-plist name))))
    (if-let (recipe (plist-get plist :recipe))
        (condition-case e
            (let (quelpa-upgrade-p)
              (quelpa recipe))
          ((debug error)
           (doom--delete-package-files name)
           (signal (car e) (cdr e))))
      (package-install name))
    (if (not (doom-package-installed-p name))
        (doom--delete-package-files name)
      (add-to-list 'package-selected-packages name nil 'eq)
      (setf (alist-get name doom-packages) plist)
      name)))

;;;###autoload
(defun doom-update-package (name &optional force-p)
  "Updates package NAME (a symbol) if it is out of date, using quelpa or
package.el as appropriate."
  (cl-check-type name symbol)
  (unless (doom-package-installed-p name)
    (error "%s isn't installed" name))
  (when (doom-package-different-backend-p name)
    (user-error "%s's backend has changed and must be uninstalled first" name))
  (when (or force-p (doom-package-outdated-p name))
    (let ((inhibit-message (not doom-debug-mode))
          (desc (doom-package-desc name)))
      (pcase (doom-package-backend name)
        (`quelpa
         (let ((name (doom-package-true-name name)))
           (condition-case e
               (let ((quelpa-upgrade-p t))
                 (quelpa (assq name quelpa-cache)))
             ((debug error)
              (doom--delete-package-files name)
              (signal (car e) (cdr e))))))
        (`elpa
         (let* ((archive (cadr (assq name package-archive-contents)))
                (packages
                 (if (package-desc-p archive)
                     (package-compute-transaction (list archive) (package-desc-reqs archive))
                   (package-compute-transaction () (list (list archive))))))
           (package-download-transaction packages))))
      (unless (doom-package-outdated-p name)
        (doom--delete-package-files desc)
        t))))

;;;###autoload
(defun doom-delete-package (name &optional force-p)
  "Uninstalls package NAME if it exists, and clears it from `quelpa-cache'."
  (cl-check-type name symbol)
  (unless (doom-package-installed-p name)
    (user-error "%s isn't installed" name))
  (let ((inhibit-message (not doom-debug-mode))
        (name (doom-package-true-name name)))
    (when-let (spec (assq name quelpa-cache))
      (delq! spec quelpa-cache)
      (quelpa-save-cache))
    (package-delete (doom-package-desc name) force-p)
    (doom--delete-package-files name)
    (not (doom-package-installed-p name))))


;;
;; Interactive commands

;;;###autoload
(defun doom/reload-packages ()
  "Reload `doom-packages', `package' and `quelpa'."
  (interactive)
  (message "Reloading packages")
  (doom-initialize-packages t)
  (message "Reloading packages...DONE"))

;;;###autoload
(defun doom/update-package (pkg)
  "Prompts the user with a list of outdated packages and updates the selected
package. Use this interactively. Use `doom-update-package' for direct
calls."
  (declare (interactive-only t))
  (interactive
   (let* ((packages (doom-get-outdated-packages))
          (selection (if packages
                         (completing-read "Update package: "
                                          (mapcar #'car packages)
                                          nil t)
                       (user-error "All packages are up to date")))
          (name (car (assoc (intern selection) package-alist))))
     (unless name
       (user-error "'%s' is already up-to-date" selection))
     (list (assq name packages))))
  (cl-destructuring-bind (package old-version new-version) pkg
    (if-let (desc (doom-package-outdated-p package))
        (let ((old-v-str (package-version-join old-version))
              (new-v-str (package-version-join new-version)))
          (if (y-or-n-p (format "%s will be updated from %s to %s. Update?"
                                package old-v-str new-v-str))
              (message "%s %s (%s => %s)"
                       (if (doom-update-package package t) "Updated" "Failed to update")
                       package old-v-str new-v-str)
            (message "Aborted")))
      (message "%s is up-to-date" package))))


;;
;; Advice

;;;###autoload
(defun doom*package-delete (desc &rest _)
  "Update `quelpa-cache' upon a successful `package-delete'."
  (let ((name (package-desc-name desc)))
    (unless (doom-package-installed-p name)
      (when-let (spec (assq name quelpa-cache))
        (setq quelpa-cache (delq spec quelpa-cache))
        (quelpa-save-cache)
        (doom--delete-package-files name)))))


;;
;; Make package.el cooperate with Doom

;; Updates QUELPA after deleting a package
;;;###autoload
(advice-add #'package-delete :after #'doom*package-delete)

;; Replace with Doom variants
;;;###autoload
(advice-add #'package-autoremove :override #'doom//autoremove)

;;;###autoload
(advice-add #'package-install-selected-packages :override #'doom//install)
