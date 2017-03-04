;;; packages.el
(provide 'doom-lib-packages)

;;;###autoload
(defun doom-refresh-packages ()
  "Refresh ELPA packages."
  (doom-initialize)
  (let ((last-refresh (persistent-soft-fetch 'last-pkg-refresh "emacs")))
    (when (or (not last-refresh)
              (> (nth 1 (time-since last-refresh)) 600))
      (package-refresh-contents)
      (persistent-soft-store 'last-pkg-refresh (current-time) "emacs"))))

;;;###autoload
(defun doom-package-backend (name)
  "Get which backend the package NAME was installed with. Can either be elpa,
quelpa or nil (if not installed)."
  (doom-initialize)
  (unless (quelpa-setup-p)
    (error "Could not initialize quelpa"))
  (cond ((let ((plist (cdr (assq name doom-packages))))
           (and (not (plist-get plist :pin))
                (or (assq name quelpa-cache)
                    (plist-get plist :recipe))))
         'quelpa)
        ((assq name package-alist)
         'elpa)))

;;;###autoload
(defun doom-package-outdated-p (name)
  "Determine whether NAME (a symbol) is outdated or not. If outdated, returns a
list, whose car is NAME, and cdr the current version list and latest version
list of the package."
  (doom-initialize)
  (when-let (pkg (assq name package-alist))
    (let* ((old-version (package-desc-version (cadr pkg)))
           (new-version
            (pcase (doom-package-backend name)
              ('quelpa
               (let ((recipe (assq name quelpa-cache))
                     (dir (expand-file-name (symbol-name name) quelpa-build-dir))
                     (inhibit-message t))
                 (if-let (ver (and (quelpa-setup-p) (quelpa-checkout recipe dir)))
                     (version-to-list ver)
                   old-version)))
              ('elpa
               (doom-refresh-packages)
               (let ((desc (cadr (assq name package-archive-contents))))
                 (when (package-desc-p desc)
                   (package-desc-version desc)))))))
      (when (and (listp old-version) (listp new-version)
                 (version-list-< old-version new-version))
        (list name old-version new-version)))))

;;;###autoload
(defun doom-get-packages (&optional backend)
  "Retrieves a list of explicitly installed packages (i.e. non-dependencies).
Each element is a cons cell, whose car is the package symbol and whose cdr is
the quelpa recipe (if any).

BACKEND can be 'quelpa or 'elpa, and will instruct this function to return only
the packages relevant to that backend.

Warning: this function is expensive; it re-evaluates all of doom's config files.
Be careful not to use it in a loop."
  (doom-initialize-packages t)
  (unless (quelpa-setup-p)
    (error "Could not initialize quelpa"))
  (delq nil
        (mapcar (lambda (pkgsym)
                  (or (assq pkgsym doom-packages)
                      (list (car (assq pkgsym package-alist)))))
                (append doom-protected-packages (mapcar 'car doom-packages)))))

;;;###autoload
(defun doom-get-dependencies-for (name)
  "Return a list of packages that depend on the package named NAME."
  (doom-initialize)
  (when-let (desc (cadr (assq name package-alist)))
    (mapcar 'package-desc-name (package--used-elsewhere-p desc nil t))))

;;;###autoload
(defun doom-get-outdated-packages ()
  "Return a list of packages that are out of date. Each element is a list,
containing (PACKAGE-SYMBOL OLD-VERSION-LIST NEW-VERSION-LIST).

Used by `doom/packages-update'."
  (delq nil (mapcar 'doom-package-outdated-p (mapcar 'car (doom-get-packages)))))

;;;###autoload
(defun doom-get-orphaned-packages ()
  "Return a list of symbols representing packages that are no longer needed or
depended on.

Used by `doom/packages-autoremove'."
  (doom-initialize-packages t)
  (let ((package-selected-packages
         (append (mapcar 'car doom-packages) doom-protected-packages)))
    (package--removable-packages)))

;;;###autoload
(defun doom-get-missing-packages ()
  "Return a list of requested packages that aren't installed or built-in. Each
element is a list whose CAR is the package symbol, and whose CDR is a plist
taken from that package's `package!' declaration.

Used by `doom/packages-install'."
  (cl-remove-if (lambda (pkgsym)
                  (or (assq (car pkgsym) package-alist)
                      (and (not (plist-get (assq (car pkgsym) doom-packages) :pin))
                           (assq (car pkgsym) package--builtins))))
                (doom-get-packages)))

;;;###autoload
(defun doom*package-delete (name &rest _)
  "Update `quelpa-cache' upon a successful `package-delete'."
  (when (and (not (package-installed-p name))
             (quelpa-setup-p)
             (assq name quelpa-cache))
    (setq quelpa-cache (assq-delete-all name quelpa-cache))
    (quelpa-save-cache)
    (let ((path (expand-file-name (symbol-name name) quelpa-build-dir)))
      (when (file-exists-p path)
        (delete-directory path t)))))

;;; Private functions
(defsubst doom--version-list-str (vlist)
  (concat (number-to-string (car vlist))
          "."
          (number-to-string (cadr vlist))))

(defsubst doom--sort-alpha (it other)
  (string-lessp (symbol-name (car it))
                (symbol-name (car other))))


;;
;; Main functions
;;

(defun doom-install-package (name &optional plist)
  "Installs package NAME with optional quelpa RECIPE (see `quelpa-recipe' for an
example; the package name can be omitted)."
  (doom-refresh-packages)
  (doom-initialize-packages)
  (when (package-installed-p name)
    (error "%s is already installed, skipping" name))
  (let ((inhibit-message (not doom-debug-mode))
        (recipe (plist-get plist :recipe)))
    (cond (recipe (quelpa recipe))
          (t (package-install name))))
  (cl-pushnew (cons name plist) doom-packages :key 'car)
  (package-installed-p name))

(defun doom-update-package (name)
  "Updates package NAME if it is out of date, using quelpa or package.el as
appropriate."
  (doom-initialize)
  (unless (package-installed-p name)
    (error "%s isn't installed" name))
  (when (doom-package-outdated-p name)
    (let ((inhibit-message (not doom-debug-mode))
          quelpa-modified-p)
      (pcase (doom-package-backend name)
        ('quelpa
         (let ((quelpa-upgrade-p t))
           (quelpa (assq name quelpa-cache))
           (setq quelpa-modified-p t)))
        ('elpa
         (let ((desc    (cadr (assq name package-alist)))
               (archive (cadr (assq name package-archive-contents))))
           (package-install-from-archive archive)
           (delete-directory (package-desc-dir desc) t))))
      (when quelpa-modified-p
        (quelpa-save-cache)))
    (version-list-=
     (package-desc-version (cadr (assq name package-alist)))
     (package-desc-version (cadr (assq name package-archive-contents))))))

(defun doom-delete-package (name &optional force-p)
  "Uninstalls package NAME if it exists, and clears it from `quelpa-cache'."
  (doom-initialize)
  (unless (package-installed-p name)
    (error "%s isn't installed" name))
  (let ((desc (cadr (assq name package-alist)))
        (inhibit-message t))
    (package-delete desc force-p))
  (not (package-installed-p name)))


;;
;; Interactive commands
;;

;;;###autoload
(defun doom/packages-install ()
  "Interactive command for installing missing packages."
  (interactive)
  (let ((packages (doom-get-missing-packages)))
    (cond ((not packages)
           (message "No packages to install!"))

          ((not (or (getenv "YES")
                    (y-or-n-p
                     (format "%s packages will be installed:\n\n%s\n\nProceed?"
                             (length packages)
                             (mapconcat (lambda (pkg)
                                          (format "+ %s (%s)"
                                                  (car pkg)
                                                  (if (plist-get (cdr pkg) :recipe)
                                                      "QUELPA"
                                                    "ELPA")))
                                        (sort (cl-copy-list packages) 'doom--sort-alpha)
                                        "\n")))))
           (message "Aborted!"))

          (t
           (message "Installing %s packages" (length packages))

           (dolist (pkg packages)
             (condition-case ex
                 (progn
                   (message "%s %s (%s)"
                            (cond ((package-installed-p (car pkg))
                                   "Skipped (already installed)")
                                  ((doom-install-package (car pkg) (cdr pkg))
                                   "Installed")
                                  (t "Failed to install"))
                            (concat (symbol-name (car pkg))
                                    (when (plist-member (cdr pkg) :pin)
                                      (format " [pinned: %s]" (plist-get (cdr pkg) :pin))))
                            (pcase (doom-package-backend (car pkg))
                              ('quelpa "QUELPA")
                              ('elpa "ELPA"))))
               (error
                (message "Error (%s): %s" (car pkg) ex))))

           (message "Finished!")
           (when noninteractive
             (message "Restart emacs for these changes to take effect."))))))

;;;###autoload
(defun doom/packages-update ()
  "Interactive command for updating packages."
  (interactive)
  (let ((packages (sort (doom-get-outdated-packages) 'doom--sort-alpha)))
    (cond ((not packages)
           (message "Everything is up-to-date"))

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
                                  (format "+ %s %s -> %s"
                                          (s-pad-right (+ max-len 2) " " (symbol-name (car pkg)))
                                          (s-pad-right 14 " " (doom--version-list-str (cadr pkg)))
                                          (doom--version-list-str (cl-caddr pkg))))
                                packages
                                "\n"))))))
           (message "Aborted!"))

          (t
           (dolist (pkg packages)
             (condition-case ex
                 (progn
                   (message "%s %s"
                            (if (doom-update-package (car pkg))
                                "Updated"
                              "Failed to update")
                            (car pkg)))
               (error
                (message "Error installing %s: %s" (car pkg) ex))))

           (message "Finished!")
           (when noninteractive
             (message "Restart emacs for these changes to take effect."))))))

;;;###autoload
(defun doom/packages-autoremove ()
  "Interactive command for auto-removing orphaned packages."
  (interactive)
  (let ((packages (doom-get-orphaned-packages)))
    (cond ((not packages)
           (message "No unused packages to remove"))

          ((not (or (getenv "YES")
                    (y-or-n-p
                     (format "%s packages will be deleted:\n\n%s\n\nProceed?"
                             (length packages)
                             (mapconcat (lambda (sym) (format "+ %s" (symbol-name sym)))
                                        (sort (cl-copy-list packages) 'string-lessp)
                                        "\n")))))
           (message "Aborted!"))

          (t
           (dolist (pkg packages)
             (condition-case ex
                 (message "%s %s"
                               (if (doom-delete-package pkg t)
                                   "Deleted"
                                 "Failed to delete")
                               pkg)
               (error
                (message "Error deleting %s: %s" pkg ex))))

           (message "Finished!")
           (when noninteractive
             (message "Restart emacs for these changes to take effect."))))))

;;;###autoload
(defalias 'doom/install-package 'package-install)

;;;###autoload
(defun doom/delete-package (package)
  "Prompts the user with a list of packages and deletes the selected package.
Use this interactively. Use `doom-delete-package' for direct calls."
  (interactive
   (progn
     (doom-initialize)
     (list (completing-read
            "Delete package: "
            (delq nil
                  (mapcar (lambda (p) (unless (package-built-in-p p) p))
                          (mapcar 'car package-alist)))
            nil t))))
  (if (package-installed-p package)
      (if (y-or-n-p (format "%s will be deleted. Confirm?" package))
          (message "%s %s"
                   (if (doom-delete-package package)
                       "Deleted"
                     "Failed to delete")
                   pkg)
        (message "Aborted"))
    (message "%s isn't installed" package)))

;;;###autoload
(defun doom/update-package (package)
  "Prompts the user with a list of outdated packages and updates the selected
package. Use this interactively. Use `doom-update-package' for direct
calls."
  (declare (interactive-only t))
  (interactive
   (let ((packages (doom-get-outdated-packages)))
     (list
      (if packages
          (completing-read "Update package: " (mapcar 'symbol-name (mapcar 'car packages)))
        (user-error "All packages are up-to-date")))))
  (if-let (desc (doom-package-outdated-p (intern package)))
      (if (y-or-n-p (format "%s will be updated from %s to %s. Update?"
                            (car desc)
                            (doom--version-list-str (cadr desc))
                            (doom--version-list-str (cl-caddr desc))))
          (message "%s %s"
                   (if (doom-update-package package)
                       "Updated"
                     "Failed to update")
                   pkg)
        (message "Aborted"))
    (message "%s is up-to-date" package)))
