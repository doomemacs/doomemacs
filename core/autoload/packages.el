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

(defmacro doom--condition-case! (&rest body)
  `(condition-case-unless-debug e
       (progn ,@body)
     ('user-error
      (print! (bold (red "  NOTICE: %s")) e))
     ('file-error
      (print! (bold (red "  FILE ERROR: %s")) (error-message-string e))
      (print! "  Trying again...")
      (quiet! (doom-refresh-packages-maybe t))
      ,@body)
     ('error
      (print! (bold (red "  FATAL ERROR: %s\n  Run again with the -d flag for details")) e))))

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
  "Get which backend the package NAME was installed with. Can either be elpa or
quelpa. Throws an error if NOERROR is nil and the package isn't installed."
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
  (doom-initialize-packages)
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
  (doom-initialize-packages)
  (and (package-installed-p name)
       (when-let* ((quelpa-recipe (assq name quelpa-cache))
                   (doom-recipe   (assq name doom-packages)))
         (not (equal (cdr quelpa-recipe)
                     (cdr (plist-get (cdr doom-recipe) :recipe)))))))

;;;###autoload
(cl-defun doom-get-packages (&key (installed 'any)
                                  (private 'any)
                                  (disabled 'any)
                                  (pinned 'any)
                                  (ignored 'any)
                                  (sort t)
                                  changed
                                  backend
                                  deps)
  "Retrieves a list of primary packages (i.e. non-dependencies). Each element is
a cons cell, whose car is the package symbol and whose cdr is the quelpa recipe
(if any).

You can build a filtering criteria using one or more of the following
properties:

  :backend BACKEND
    Can be 'quelpa, 'elpa or 'emacs
  :installed BOOL
    Only return installed packages (t) or uninstalled packages (nil)
  :private BOOL
    Only return private packages (t) or non-private packages (nil)
  :disabled BOOL
    Only return packages that are disabled (t) or otherwise (nil)
  :ignored BOOL
    Only return packages that are ignored (t) or otherwise (nil)
  :pinned BOOL|ARCHIVE
    Only return packages that are pinned (t), not pinned (nil) or pinned to a
    specific archive (stringp)
  :deps BOOL
    Includes the package's dependencies (t).

The resulting list is sorted unless :sort nil is passed to this function.

Warning: this function is expensive, as it re-evaluates your all packages.el
files."
  (doom-initialize-packages)
  (cl-remove-duplicates
   (cl-loop with packages = (append (mapcar #'list doom-core-packages)
                                    doom-packages)
            for (sym . plist)
            in (if sort
                   (cl-sort (copy-sequence packages) #'string-lessp :key #'car)
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
                        (if private
                            (plist-get plist :private)
                          (not (plist-get plist :private))))
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
                     collect (cons pkg (cdr (assq pkg doom-packages)))))
   :key #'car))

;;;###autoload
(defun doom-get-package-alist ()
  "Returns a list of all desired packages, their dependencies and their desc
objects, in the order of their `package! blocks.'"
  (doom-initialize-packages)
  (cl-remove-duplicates
   (cl-loop for name in (append doom-core-packages (mapcar #'car doom-packages))
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
  (doom-initialize-packages t)
  (doom-refresh-packages-maybe doom-debug-mode)
  (require 'async)
  (let (quelpa-pkgs elpa-pkgs)
    ;; Separate quelpa from elpa packages
    (dolist (pkg (mapcar #'car package-alist))
      (when (and (or (not (doom-package-prop pkg :freeze 'eval))
                     include-frozen-p)
                 (not (doom-package-prop pkg :ignore 'eval))
                 (not (doom-package-different-backend-p pkg)))
        (push pkg
              (if (eq (doom-package-backend pkg) 'quelpa)
                  quelpa-pkgs
                elpa-pkgs))))
    ;; The bottleneck in this process is quelpa's version checks, so check them
    ;; asynchronously.
    (let (futures)
      (dolist (pkg quelpa-pkgs)
        (when doom-debug-mode
          (message "New thread for: %s" pkg))
        (push (async-start
               `(lambda ()
                  (let ((gc-cons-threshold ,doom-gc-cons-upper-limit)
                        (doom-init-p t)
                        (noninteractive t)
                        (load-path ',load-path)
                        (package-alist ',package-alist)
                        (package-archive-contents ',package-archive-contents)
                        (package-selected-packages ',package-selected-packages)
                        (doom-packages ',doom-packages)
                        (doom-modules ',doom-modules)
                        (quelpa-cache ',quelpa-cache)
                        (user-emacs-directory ,user-emacs-directory)
                        doom-private-dir)
                    (load ,(expand-file-name "core.el" doom-core-dir))
                    (load ,(expand-file-name "autoload/packages.el" doom-core-dir))
                    (require 'package)
                    (require 'quelpa)
                    (doom-package-outdated-p ',pkg))))
              futures))
      (delq nil
            (append (mapcar #'doom-package-outdated-p elpa-pkgs)
                    (mapcar #'async-get (reverse futures)))))))

;;;###autoload
(defun doom-get-orphaned-packages ()
  "Return a list of symbols representing packages that are no longer needed or
depended on.

Used by `doom-packages-autoremove'."
  (let ((package-selected-packages
         (mapcar #'car (doom-get-packages :ignored nil :disabled nil))))
    (append (package--removable-packages)
            (cl-loop for pkg in package-selected-packages
                     if (and (doom-package-different-backend-p pkg)
                             (not (package-built-in-p pkg)))
                     collect pkg))))

;;;###autoload
(defun doom-get-missing-packages (&optional include-ignored-p)
  "Return a list of requested packages that aren't installed or built-in, but
are enabled (with a `package!' directive). Each element is a list whose CAR is
the package symbol, and whose CDR is a plist taken from that package's
`package!' declaration.

If INCLUDE-IGNORED-P is non-nil, includes missing packages that are ignored,
i.e. they have an :ignore property.

Used by `doom-packages-install'."
  (doom-initialize-packages)
  (cl-loop for (name . plist)
           in (doom-get-packages :ignored (if include-ignored-p 'any)
                                 :disabled nil
                                 :deps t
                                 :sort nil)
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
  (doom-initialize-packages)
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
      (setf (alist-get name doom-packages) plist)
      name)))

;;;###autoload
(defun doom-update-package (name &optional force-p)
  "Updates package NAME (a symbol) if it is out of date, using quelpa or
package.el as appropriate."
  (cl-check-type name symbol)
  (doom-initialize-packages)
  (unless (package-installed-p name)
    (user-error "%s isn't installed" name))
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
  (doom-initialize-packages)
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
(defun doom/update-package (pkg)
  "Prompts the user with a list of outdated packages and updates the selected
package. Use this interactively. Use `doom-update-package' for direct
calls."
  (declare (interactive-only t))
  (interactive
   (let* ((packages (doom-get-outdated-packages))
          (package (if packages
                       (completing-read "Update package: "
                                        (mapcar #'car packages)
                                        nil t)
                     (user-error "All packages are up to date"))))
     (list (cdr (assq (car (assoc package package-alist)) packages)))))
  (doom-initialize-packages)
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
  (doom-initialize-packages)
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
(advice-add #'package-autoremove :override (λ! (doom-packages-autoremove current-prefix-arg)))

;;;###autoload
(advice-add #'package-install-selected-packages :override (λ! (doom-packages-install current-prefix-arg)))

