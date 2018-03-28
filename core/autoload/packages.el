;;; core/autoload/packages.el -*- lexical-binding: t; -*-

(load! cache)
(require 'use-package)
(require 'quelpa)
(require 'package)
(require 'async)

;;; Private functions
(defsubst doom--sort-alpha (it other)
  (string-lessp (symbol-name (car it))
                (symbol-name (car other))))

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
  `(condition-case-unless-debug ex
       (condition-case ex2
           (progn ,@body)
         ('file-error
          (message! (bold (red "  FILE ERROR: %s" (error-message-string ex2))))
          (message! "  Trying again...")
          (quiet! (doom-refresh-packages-maybe t))
          ,@body))
     ('user-error
      (message! (bold (red "  ERROR: %s" ex))))
     ('error
      (doom--refresh-pkg-cache)
      (message! (bold (red "  FATAL ERROR: %s" ex))))))

(defun doom--refresh-pkg-cache ()
  "Clear the cache for `doom-refresh-packages-maybe'."
  (setq doom--refreshed-p nil)
  (doom-cache-set 'last-pkg-refresh nil))


;;
;; Library
;;

;;;###autoload
(defun doom-refresh-packages-maybe (&optional force-p)
  "Refresh ELPA packages, if it hasn't been refreshed recently."
  (when force-p
    (doom--refresh-pkg-cache))
  (unless (or (doom-cache-get 'last-pkg-refresh)
              doom--refreshed-p)
    (condition-case-unless-debug ex
        (progn
          (message "Refreshing package archives")
          (package-refresh-contents)
          (doom-cache-set 'last-pkg-refresh t 900))
    ('error
     (doom--refresh-pkg-cache)
     (message "Failed to refresh packages: (%s) %s"
              (car ex) (error-message-string ex))))))

;;;###autoload
(defun doom-package-backend (name &optional noerror)
  "Get which backend the package NAME was installed with. Can either be elpa or
quelpa. Throws an error if NOERROR is nil and the package isn't installed."
  (cl-assert (symbolp name) t)
  (cond ((assq name quelpa-cache)
         'quelpa)
        ((assq name package-alist)
         'elpa)
        ((package-built-in-p name)
         'emacs)
        ((not noerror)
         (error "%s package is not installed" name))))

;;;###autoload
(defun doom-package-outdated-p (name)
  "Determine whether NAME (a symbol) is outdated or not. If outdated, returns a
list, whose car is NAME, and cdr the current version list and latest version
list of the package."
  (cl-assert (symbolp name) t)
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
      (when (and (listp old-version) (listp new-version)
                 (version-list-< old-version new-version))
        (list name old-version new-version)))))

;;;###autoload
(defun doom-package-prop (name prop)
  "Return PROPerty in NAME's plist."
  (cl-assert (symbolp name) t)
  (cl-assert (keywordp prop) t)
  (plist-get (cdr (assq name doom-packages)) prop))

;;;###autoload
(defun doom-package-different-backend-p (name)
  "Return t if a package named NAME (a symbol) has a new backend than what it
was installed with. Returns nil otherwise, or if package isn't installed."
  (cl-assert (symbolp name) t)
  (and (package-installed-p name)
       (let* ((plist (cdr (assq name doom-packages)))
              (old-backend (doom-package-backend name 'noerror))
              (new-backend (if (plist-get plist :recipe) 'quelpa 'elpa)))
         (not (eq old-backend new-backend)))))

;;;###autoload
(defun doom-package-different-recipe-p (name)
  "Return t if a package named NAME (a symbol) has a different recipe than it
was installed with."
  (cl-assert (symbolp name) t)
  (when (package-installed-p name)
    (let ((quelpa-recipe (assq name quelpa-cache))
          (doom-recipe (assq name doom-packages)))
      (and quelpa-recipe doom-recipe
           (not (equal (cdr quelpa-recipe)
                       (cdr (plist-get (cdr doom-recipe) :recipe))))))))

;;;###autoload
(defun doom-get-packages (&optional installed-only-p)
  "Retrieves a list of explicitly installed packages (i.e. non-dependencies).
Each element is a cons cell, whose car is the package symbol and whose cdr is
the quelpa recipe (if any).

BACKEND can be 'quelpa or 'elpa, and will instruct this function to return only
the packages relevant to that backend.

Warning: this function is expensive; it re-evaluates all of doom's config files.
Be careful not to use it in a loop.

If INSTALLED-ONLY-P, only return packages that are installed."
  (doom-initialize-packages t)
  (cl-loop with packages = (append doom-core-packages (mapcar #'car doom-packages))
           for sym in (cl-delete-duplicates packages)
           if (and (or (not installed-only-p)
                       (package-installed-p sym))
                   (or (assq sym doom-packages)
                       (and (assq sym package-alist)
                            (list sym))))
           collect it))

;;;###autoload
(defun doom-get-depending-on (name)
  "Return a list of packages that depend on the package named NAME."
  (when (package-built-in-p name)
    (error "Can't get the dependency tree for built-in packages"))
  (when-let* ((desc (cadr (assq name package-alist))))
    (mapcar #'package-desc-name (package--used-elsewhere-p desc nil t))))

;;;###autoload
(defun doom-get-dependencies-for (name &optional only)
  "Return a list of dependencies for a package."
  (when (package-built-in-p name)
    (error "Can't get the dependency tree for built-in packages"))
  (package--get-deps name only))

;;;###autoload
(defun doom-get-outdated-packages (&optional include-frozen-p)
  "Return a list of packages that are out of date. Each element is a list,
containing (PACKAGE-SYMBOL OLD-VERSION-LIST NEW-VERSION-LIST).

If INCLUDE-FROZEN-P is non-nil, check frozen packages as well.

Used by `doom//packages-update'."
  (doom-initialize-packages t)
  (require 'async)
  (let (quelpa-pkgs elpa-pkgs)
    ;; Separate quelpa from elpa packages
    (dolist (pkg (mapcar #'car package-alist))
      (when (and (or (not (doom-package-prop pkg :freeze))
                     include-frozen-p)
                 (not (doom-package-prop pkg :ignore))
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
                  (setq user-emacs-directory ,user-emacs-directory)
                  (let ((noninteractive t))
                    (load ,(expand-file-name "core.el" doom-core-dir)))
                  (setq doom-packages ',doom-packages
                        doom-modules ',doom-modules
                        quelpa-cache ',quelpa-cache)
                  (doom-package-outdated-p ',pkg)))
              futures))
      (delq nil
            (append (mapcar #'doom-package-outdated-p elpa-pkgs)
                    (mapcar #'async-get (reverse futures)))))))

;;;###autoload
(defun doom-get-orphaned-packages ()
  "Return a list of symbols representing packages that are no longer needed or
depended on.

Used by `doom//packages-autoremove'."
  (doom-initialize-packages t)
  (let ((package-selected-packages
         (append (mapcar #'car doom-packages) doom-core-packages)))
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

Used by `doom//packages-install'."
  (doom-initialize-packages 'internal)
  (cl-loop for desc in (doom-get-packages)
           for (name . plist) = desc
           if (and (or include-ignored-p
                       (not (plist-get plist :ignore)))
                   (or (plist-get plist :pin)
                       (not (assq name package--builtins)))
                   (or (not (assq name package-alist))
                       (doom-package-different-backend-p name)
                       (doom-package-different-recipe-p name)))
           collect desc))


;;
;; Main functions
;;

(defun doom-install-package (name &optional plist)
  "Installs package NAME with optional quelpa RECIPE (see `quelpa-recipe' for an
example; the package name can be omitted)."
  (doom-initialize-packages)
  (when (and (package-installed-p name)
             (not (package-built-in-p name)))
    (if (or (doom-package-different-backend-p name)
            (doom-package-different-recipe-p name))
        (doom-delete-package name t)
      (user-error "%s is already installed" name)))
  (let* ((inhibit-message (not doom-debug-mode))
         (plist (or plist (cdr (assq name doom-packages))))
         (recipe (plist-get plist :recipe))
         quelpa-upgrade-p)
    (if recipe
        (condition-case-unless-debug _
            (quelpa recipe)
          ('error
           (let ((pkg-build-dir (expand-file-name (symbol-name name) quelpa-build-dir)))
             (when (file-directory-p pkg-build-dir)
               (delete-directory pkg-build-dir t)))))
      (package-install name))
    (when (package-installed-p name)
      (cl-pushnew (cons name plist) doom-packages :test #'eq :key #'car)
      t)))

(defun doom-update-package (name &optional force-p)
  "Updates package NAME (a symbol) if it is out of date, using quelpa or
package.el as appropriate."
  (unless (package-installed-p name)
    (user-error "%s isn't installed" name))
  (when (doom-package-different-backend-p name)
    (user-error "%s's backend has changed and must be uninstalled first" name))
  (when (or force-p (doom-package-outdated-p name))
    (let ((inhibit-message (not doom-debug-mode))
          (desc (cadr (assq name package-alist))))
      (pcase (doom-package-backend name)
        ('quelpa
         (let ((quelpa-upgrade-p t))
           (quelpa (assq name quelpa-cache))))
        ('elpa
         (let* ((archive (cadr (assq name package-archive-contents)))
                (packages
                 (if (package-desc-p archive)
                     (package-compute-transaction (list archive) (package-desc-reqs archive))
                   (package-compute-transaction () (list (list archive))))))
           (package-download-transaction packages))))
      (unless (doom-package-outdated-p name)
        (when-let* ((old-dir (package-desc-dir desc)))
          (when (file-directory-p old-dir)
            (delete-directory old-dir t)))
        t))))

(defun doom-delete-package (name &optional force-p)
  "Uninstalls package NAME if it exists, and clears it from `quelpa-cache'."
  (unless (package-installed-p name)
    (user-error "%s isn't installed" name))
  (let ((inhibit-message (not doom-debug-mode))
        quelpa-p)
    (when (assq name quelpa-cache)
      (map-delete quelpa-cache name)
      (quelpa-save-cache)
      (setq quelpa-p t))
    (package-delete (cadr (assq name package-alist)) force-p)
    (unless (package-installed-p name)
      (let ((pkg-build-dir (expand-file-name (symbol-name name) quelpa-build-dir)))
        (when (and quelpa-p (file-directory-p pkg-build-dir))
          (delete-directory pkg-build-dir t)))
      t)))


;;
;; Batch/interactive commands
;;

;;;###autoload
(defun doom//packages-install ()
  "Interactive command for installing missing packages."
  (interactive)
  (message! "Looking for packages to install...")
  (let ((packages (reverse (doom-get-missing-packages))))
    (cond ((not packages)
           (message! (green "No packages to install!")))

          ((not (or (getenv "YES")
                    (y-or-n-p
                     (format "%s packages will be installed:\n\n%s\n\nProceed?"
                             (length packages)
                             (mapconcat
                              (lambda (pkg)
                                (format "+ %s (%s)"
                                        (car pkg)
                                        (cond ((doom-package-different-recipe-p (car pkg))
                                               "new recipe")
                                              ((doom-package-different-backend-p (car pkg))
                                               (if (plist-get (cdr pkg) :recipe)
                                                   "ELPA -> QUELPA"
                                                 "QUELPA -> ELPA"))
                                              ((plist-get (cdr pkg) :recipe)
                                               "QUELPA")
                                              (t
                                               "ELPA"))))
                              (sort (cl-copy-list packages) #'doom--sort-alpha)
                              "\n")))))
           (message! (yellow "Aborted!")))

          (t
           (doom-refresh-packages-maybe doom-debug-mode)
           (dolist (pkg packages)
             (message! "Installing %s" (car pkg))
             (doom--condition-case!
              (message! "%s%s"
                        (cond ((and (package-installed-p (car pkg))
                                    (not (doom-package-different-backend-p (car pkg)))
                                    (not (doom-package-different-recipe-p (car pkg))))
                               (dark (white "⚠ ALREADY INSTALLED")))
                              ((doom-install-package (car pkg) (cdr pkg))
                               (green "✓ DONE"))
                              (t
                               (red "✕ FAILED")))
                        (if (plist-member (cdr pkg) :pin)
                            (format " [pinned: %s]" (plist-get (cdr pkg) :pin))
                          ""))))

           (message! (bold (green "Finished!")))
           (doom//reload-load-path)))))

;;;###autoload
(defun doom//packages-update ()
  "Interactive command for updating packages."
  (interactive)
  (message! "Looking for outdated packages...")
  (doom-refresh-packages-maybe doom-debug-mode)
  (let ((packages (sort (doom-get-outdated-packages) #'doom--sort-alpha)))
    (cond ((not packages)
           (message! (green "Everything is up-to-date")))

          ((not (or (getenv "YES")
                    (y-or-n-p
                     (format "%s packages will be updated:\n\n%s\n\nProceed?"
                             (length packages)
                             (let ((max-len
                                    (or (car (sort (mapcar (lambda (it) (length (symbol-name (car it)))) packages)
                                                   (lambda (it other) (> it other))))
                                        10)))
                               (mapconcat
                                (lambda (pkg)
                                  (format (format "+ %%-%ds %%-%ds -> %%s" (+ max-len 2) 14)
                                          (symbol-name (car pkg))
                                          (package-version-join (cadr pkg))
                                          (package-version-join (cl-caddr pkg))))
                                packages
                                "\n"))))))
           (message! (yellow "Aborted!")))

          (t
           (dolist (pkg packages)
             (message! "Updating %s" (car pkg))
             (doom--condition-case!
              (message!
               (let ((result (doom-update-package (car pkg) t)))
                 (color (if result 'green 'red)
                        (if result "✓ DONE" "✕ FAILED"))))))

           (message! (bold (green "Finished!")))
           (doom//reload-load-path)))))

;;;###autoload
(defun doom//packages-autoremove ()
  "Interactive command for auto-removing orphaned packages."
  (interactive)
  (message! "Looking for orphaned packages...")
  (let ((packages (doom-get-orphaned-packages)))
    (cond ((not packages)
           (message! (green "No unused packages to remove")))

          ((not
            (or (getenv "YES")
                (y-or-n-p
                 (format
                  "%s packages will be deleted:\n\n%s\n\nProceed?"
                  (length packages)
                  (mapconcat
                   (lambda (sym)
                     (format "+ %s (%s)" sym
                             (let ((backend (doom-package-backend sym)))
                               (if (doom-package-different-backend-p sym)
                                   (if (eq backend 'quelpa)
                                       "QUELPA->ELPA"
                                     "ELPA->QUELPA")
                                 (upcase (symbol-name backend))))))
                   (sort (cl-copy-list packages) #'string-lessp)
                   "\n")))))
           (message! (yellow "Aborted!")))

          (t
           (dolist (pkg packages)
             (doom--condition-case!
              (message!
               (let ((result (doom-delete-package pkg t)))
                 (color (if result 'green 'red)
                        "%s %s"
                        (if result "✓ Removed" "✕ Failed to remove")
                        pkg)))))

           (message! (bold (green "Finished!")))
           (doom//reload-load-path)))))


;;
;; Interactive commands
;;

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
;;

;;;###autoload
(defun doom*package-delete (desc &rest _)
  "Update `quelpa-cache' upon a successful `package-delete'."
  (doom-initialize-packages)
  (let ((name (package-desc-name desc)))
    (when (and (not (package-installed-p name))
               (assq name quelpa-cache))
      (map-delete quelpa-cache name)
      (quelpa-save-cache)
      (let ((path (expand-file-name (symbol-name name) quelpa-build-dir)))
        (when (file-exists-p path)
          (delete-directory path t))))))

