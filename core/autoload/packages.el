;;; core/autoload/packages.el -*- lexical-binding: t; -*-

(load! "cache")

;;; Private functions
(defun doom--packages-choose (prompt)
  (let ((table (cl-loop for pkg in package-alist
                        unless (package-built-in-p (cdr pkg))
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


;;
;; Library

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

;;;###autoload
(defun doom-package-backend (name &optional noerror)
  "Get which backend the package NAME was installed with. Can either be elpa,
quelpa or emacs (built-in). Throws an error if NOERROR is nil and the package
isn't installed."
  (cl-check-type name symbol)
  (cond ((assq name quelpa-cache)  'quelpa)
        ((assq name package-alist) 'elpa)
        ((package-built-in-p name) 'emacs)
        ((not noerror) (error "%s package is not installed" name))))

;;;###autoload
(defun doom-package-outdated-p (name)
  "Determine whether NAME (a symbol) is outdated or not. If outdated, returns a
list, whose car is NAME, and cdr the current version list and latest version
list of the package."
  (cl-check-type name symbol)
  (when-let* ((desc (cadr (assq name package-alist))))
    (let* ((old-version (package-desc-version desc))
           (new-version
            (pcase (doom-package-backend name)
              ('quelpa
               (let ((recipe (plist-get (cdr (assq name doom-packages)) :recipe))
                     (dir (expand-file-name (symbol-name name) quelpa-build-dir))
                     (inhibit-message (not doom-debug-mode))
                     (quelpa-upgrade-p t))
                 (if-let* ((ver (quelpa-checkout recipe dir)))
                     (version-to-list ver)
                   old-version)))
              ('elpa
               (let ((desc (cadr (assq name package-archive-contents))))
                 (when (package-desc-p desc)
                   (package-desc-version desc)))))))
      (unless (and (listp old-version) (listp new-version))
        (error "Couldn't get version for %s" name))
      (when (version-list-< old-version new-version)
        (list name old-version new-version)))))

;;;###autoload
(defun doom-package-installed-p (name)
  "TODO"
  (and (package-installed-p name)
       (when-let* ((desc (cadr (assq name package-alist))))
         (let ((dir (package-desc-dir desc)))
           (file-directory-p dir)))))

;;;###autoload
(defun doom-package-prop (name prop &optional eval)
  "Return PROPerty in NAME's plist."
  (cl-check-type name symbol)
  (cl-check-type prop keyword)
  (let ((value (plist-get (cdr (assq name doom-packages)) prop)))
    (if eval (eval value) value)))

;;;###autoload
(defun doom-package-different-backend-p (name)
  "Return t if a package named NAME (a symbol) has a new backend than what it
was installed with. Returns nil otherwise, or if package isn't installed."
  (cl-check-type name symbol)
  (and (package-installed-p name)
       (let* ((plist (cdr (assq name doom-packages)))
              (old-backend (doom-package-backend name 'noerror))
              (new-backend (if (plist-get plist :recipe) 'quelpa 'elpa)))
         (not (eq old-backend new-backend)))))

;;;###autoload
(defun doom-package-different-recipe-p (name)
  "Return t if a package named NAME (a symbol) has a different recipe than it
was installed with."
  (cl-check-type name symbol)
  (and (package-installed-p name)
       (when-let* ((quelpa-recipe (assq name quelpa-cache))
                   (doom-recipe   (assq name doom-packages)))
         (not (equal (cdr quelpa-recipe)
                     (cdr (plist-get (cdr doom-recipe) :recipe)))))))

;;;###autoload
(cl-defun doom-find-packages (&key (installed 'any)
                                   (private 'any)
                                   (disabled 'any)
                                   (pinned 'any)
                                   (ignored 'any)
                                   (core 'any)
                                   sort
                                   changed
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

The resulting list is sorted unless :sort nil is passed to this function.

Warning: this function is expensive, as it re-evaluates your all packages.el
files."
  (cl-loop with packages = doom-packages
           for (sym . plist)
           in (if sort
                  (cl-sort (copy-sequence doom-packages) #'string-lessp :key #'car)
                packages)
           if (and (or (not backend)
                       (eq (doom-package-backend sym t) backend))
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
           and if (and deps (not (package-built-in-p sym)))
           nconc
           (cl-loop for pkg in (doom-get-dependencies-for sym 'recursive 'noerror)
                    if (or (eq installed 'any)
                           (if installed
                               (doom-package-installed-p pkg)
                             (not (doom-package-installed-p pkg))))
                    collect (cons pkg (cdr (assq pkg doom-packages))))))

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
                               :match "/packages\\.el$"))
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
  (unless (package-built-in-p name)
    (if-let* ((desc (cadr (assq name package-alist))))
        (mapcar #'package-desc-name (package--used-elsewhere-p desc nil t))
      (unless noerror
        (error "Couldn't find %s, is it installed?" name)))))

;;;###autoload
(defun doom-get-dependencies-for (name &optional recursive noerror)
  "Return a list of dependencies for a package."
  (cl-check-type name symbol)
  ;; can't get dependencies for built-in packages
  (unless (package-built-in-p name)
    (if-let* ((desc (cadr (assq name package-alist))))
        (let* ((deps (mapcar #'car (package-desc-reqs desc)))
               (deps (cl-remove-if #'package-built-in-p deps)))
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
           when (and (or (not (doom-package-prop package :freeze 'eval))
                         include-frozen-p)
                     (not (doom-package-prop package :ignore 'eval))
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
    (append (package--removable-packages)
            (cl-loop for pkg in package-selected-packages
                     if (and (doom-package-different-backend-p pkg)
                             (not (package-built-in-p pkg)))
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
           if (and (or (plist-get plist :pin)
                       (not (package-built-in-p name)))
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
  (when (and (package-installed-p name)
             (not (package-built-in-p name)))
    (if (or (doom-package-different-backend-p name)
            (doom-package-different-recipe-p name))
        (doom-delete-package name t)
      (user-error "%s is already installed" name)))
  (let* ((inhibit-message (not doom-debug-mode))
         (plist (or plist (cdr (assq name doom-packages)))))
    (if-let* ((recipe (plist-get plist :recipe)))
        (condition-case e
            (let (quelpa-upgrade-p)
              (quelpa recipe))
          ((debug error)
           (doom--delete-package-files name)
           (signal (car e) (cdr e))))
      (package-install name))
    (if (not (package-installed-p name))
        (doom--delete-package-files name)
      (add-to-list 'package-selected-packages name nil 'eq)
      (setf (alist-get name doom-packages) plist)
      name)))

;;;###autoload
(defun doom-update-package (name &optional force-p)
  "Updates package NAME (a symbol) if it is out of date, using quelpa or
package.el as appropriate."
  (cl-check-type name symbol)
  (unless (package-installed-p name)
    (error "%s isn't installed" name))
  (when (doom-package-different-backend-p name)
    (user-error "%s's backend has changed and must be uninstalled first" name))
  (when (or force-p (doom-package-outdated-p name))
    (let ((inhibit-message (not doom-debug-mode))
          (desc (cadr (assq name package-alist))))
      (pcase (doom-package-backend name)
        (`quelpa
         (condition-case e
             (let ((quelpa-upgrade-p t))
               (quelpa (assq name quelpa-cache)))
           ((debug error)
            (doom--delete-package-files name)
            (signal (car e) (cdr e)))))
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
  (unless (package-installed-p name)
    (user-error "%s isn't installed" name))
  (let ((inhibit-message (not doom-debug-mode))
        (spec (assq name quelpa-cache))
        quelpa-p)
    (when spec
      (setq quelpa-cache (delq spec quelpa-cache))
      (quelpa-save-cache)
      (setq quelpa-p t))
    (package-delete (cadr (assq name package-alist)) force-p)
    (doom--delete-package-files name)
    (not (package-installed-p name))))


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
    (if-let* ((desc (doom-package-outdated-p package)))
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
    (unless (package-installed-p name)
      (when-let* ((spec (assq name quelpa-cache)))
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

;; Don't save `package-selected-packages' to `custom-file'
;;;###autoload
(advice-add #'package--save-selected-packages :override
            (lambda (&optional value) (if value (setq package-selected-packages value))))
