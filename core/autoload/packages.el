;;; packages.el
(provide 'doom-lib-packages)

(defvar doom-packages-last-refresh nil
  "A timestamp indicating the last time `package-refresh-contents' was run.")

;;;###autoload
(defun doom-refresh-packages ()
  "Refresh ELPA packages."
  (doom-initialize)
  (let ((refresh-cache (f-expand "last-pkg-refresh" doom-cache-dir)))
    (when (and (not doom-packages-last-refresh)
               (f-exists-p refresh-cache))
      (setq doom-packages-last-refresh (read (f-read refresh-cache))))
    (when (or (not doom-packages-last-refresh)
              (> (nth 1 (time-since doom-packages-last-refresh)) 600))
      (package-refresh-contents)
      (setq doom-packages-last-refresh (current-time))
      (f-write (pp-to-string doom-packages-last-refresh) 'utf-8 refresh-cache))))

;;;###autoload
(defun doom-package-backend (name)
  "Get which backend the package NAME was installed with. Can either be elpa,
quelpa or nil (if not installed)."
  (doom-initialize)
  (unless (quelpa-setup-p)
    (error "Could not initialize quelpa"))
  (cond ((or (assq name quelpa-cache)
             (plist-get (cdr (assq name doom-packages)) :recipe))
         'quelpa)
        ((assq name package-alist)
         'elpa)))

;;;###autoload
(defun doom-package-outdated-p (name)
  "Determine whether NAME (a symbol) is outdated or not. If outdated, returns a
list, whose car is NAME, and cdr the current version list and latest version
list of the package."
  (doom-refresh-packages)
  (-when-let (pkg (assq name package-alist))
    (let* ((old-version (package-desc-version (cadr pkg)))
           (new-version
            (pcase (doom-package-backend name)
              ('quelpa
               (let ((recipe (assq name quelpa-cache))
                     (dir (f-expand (symbol-name name) quelpa-build-dir))
                     (inhibit-message t))
                 (-if-let (ver (and (quelpa-setup-p) (quelpa-checkout recipe dir)))
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
(defun doom-get-packages (&optional backend)
  "Retrieves a list of explicitly installed packages (i.e. non-dependencies).
Each element is a cons cell, whose car is the package symbol and whose cdr is
the quelpa recipe (if any).

BACKEND can be 'quelpa or 'elpa, and will instruct this function to return only
the packages relevant to that backend."
  (doom-initialize-packages t)
  (unless (quelpa-setup-p)
    (error "Could not initialize quelpa"))
  (-non-nil
   (--map (or (assq it doom-packages)
              (list (car (assq it package-alist))))
          (append doom-protected-packages
                  (mapcar 'car doom-packages)))))

;;;###autoload
(defun doom-get-outdated-packages ()
  "Return a list of packages that are out of date. Each element is a list,
containing (PACKAGE-SYMBOL OLD-VERSION-LIST NEW-VERSION-LIST).

Used by `doom/packages-update'."
  (-non-nil (--map (doom-package-outdated-p (car it))
                   (doom-get-packages))))

;;;###autoload
(defun doom-get-orphaned-packages ()
  "Return a list of symbols representing packages that are no longer needed or
depended on.

Used by `doom/packages-autoremove'."
  (doom-initialize-packages t)
  (let ((package-selected-packages (append (mapcar 'car doom-packages) doom-protected-packages)))
    (package--removable-packages)))

;;;###autoload
(defun doom-get-missing-packages ()
  "Return a list of packages that aren't installed, but need to be. Each element
is a list whose CAR is the package symbol, and whose CDR is a plist taken from
that package's `@package' declaration.

Used by `doom/packages-install'."
  (--remove (assq (car it) package-alist) (doom-get-packages)))

;;;###autoload
(defun doom*package-delete (name)
  "Update `quelpa-cache' upon a successful `package-delete'."
  (when (and (not (package-installed-p name))
             (quelpa-setup-p)
             (assq name quelpa-cache))
    (setq quelpa-cache (assq-delete-all name quelpa-cache))
    (quelpa-save-cache)
    (let ((path (f-expand (symbol-name name) quelpa-build-dir)))
      (when (f-exists-p path)
        (delete-directory path t)))))


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
  (doom-refresh-packages)
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

(defun doom-delete-package (name)
  "Uninstalls package NAME if it exists, and clears it from `quelpa-cache'."
  (doom-initialize)
  (unless (package-installed-p name)
    (error "%s isn't installed" name))
  (let ((desc (cadr (assq name package-alist))))
    (package-delete desc))
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

          ((not (y-or-n-p
                 (format "%s packages will be installed:\n\n%s\n\nProceed?"
                         (length packages)
                         (mapconcat (lambda (pkg)
                                      (format "+ %s (%s)"
                                              (car pkg)
                                              (if (plist-get (cdr pkg) :recipe)
                                                  "QUELPA"
                                                "ELPA")))
                                    (--sort (string-lessp (symbol-name (car it))
                                                          (symbol-name (car other)))
                                            packages)
                                    "\n"))))
           (message "Aborted!"))

          (t
           (doom-message "Installing %s packages" (length packages))

           (dolist (pkg packages)
             (condition-case ex
                 (doom-message "%s %s (%s)"
                               (cond ((package-installed-p (car pkg))
                                      "Skipped (already installed)")
                                     ((doom-install-package (car pkg) (cdr pkg))
                                      "Installed")
                                     (t "Failed to install"))
                               (car pkg)
                               (cond ((cdr pkg) "QUELPA")
                                     (t "ELPA")))
               (error
                (doom-message "Error (%s): %s" (car pkg) ex))))

           (doom-message "Finished!")))))

;;;###autoload
(defun doom/packages-update ()
  "Interactive command for updating packages."
  (interactive)
  (let ((packages (doom-get-outdated-packages)))
    (cond ((not packages)
           (message "Everything is up-to-date"))

          ((not (y-or-n-p
                 (format "%s packages will be updated:\n\n%s\n\nProceed?"
                         (length packages)
                         (let ((-max-len (or (-max (--map (length (symbol-name (car it))) packages)) 10)))
                           (mapconcat
                            (lambda (pkg)
                              (format "+ %s %s -> %s"
                                      (s-pad-right (+ -max-len 2) " " (symbol-name (car pkg)))
                                      (s-pad-right 14 " " (doom--version-list-str (cadr pkg)))
                                      (doom--version-list-str (caddr pkg))))
                            (--sort (string-lessp (symbol-name (car it))
                                                  (symbol-name (car other)))
                                    packages)
                            "\n")))))
           (message "Aborted!"))

          (t
           (dolist (pkg packages)
             (condition-case ex
                 (doom-message "%s %s"
                               (if (doom-update-package (car pkg))
                                   "Updated"
                                 "Failed to update")
                               (car pkg))
               (error
                (doom-message "Error installing %s: %s" (car pkg) ex))))

           (doom-message "Finished!")))))

;;;###autoload
(defun doom/packages-autoremove ()
  "Interactive command for auto-removing orphaned packages."
  (interactive)
  (let ((packages (doom-get-orphaned-packages)))
    (cond ((not packages)
           (message "No unused packages to remove"))

          ((not (y-or-n-p
                 (format "%s packages will be deleted:\n\n%s\n\nProceed?"
                         (length packages)
                         (mapconcat (lambda (sym) (format "+ %s" (symbol-name sym)))
                                    (-sort 'string-lessp packages)
                                    "\n"))))
           (message "Aborted!"))

          (t
           (dolist (pkg packages)
             (condition-case ex
                 (doom-message "%s %s"
                               (if (doom-delete-package pkg)
                                   "Deleted"
                                 "Failed to delete")
                               pkg)
               (error
                (doom-message "Error deleting %s: %s" pkg ex))))

           (doom-message "Finished!")))))

(defun doom--version-list-str (vlist)
  (concat (number-to-string (car vlist))
          "."
          (number-to-string (cadr vlist))))

;;;###autoload
(defalias 'doom/install-package 'package-install)

;;;###autoload
(defun doom/delete-package (package)
  "Prompts the user with a list of packages and deletes the selected package.
Use this interactively. Use `doom-delete-package' for direct calls."
  (interactive
   (list (completing-read "Delete package: " (doom-get-packages))))
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
          (completing-read "Update package: " (--map (symbol-name (car it)) packages))
        (user-error "All packages are up-to-date")))))
  (-if-let (desc (doom-package-outdated-p (intern package)))
      (if (y-or-n-p (format "%s will be updated from %s to %s. Update?"
                            (car desc)
                            (doom--version-list-str (cadr desc))
                            (doom--version-list-str (caddr desc))))
          (message "%s %s"
                   (if (doom-update-package package)
                       "Updated"
                     "Failed to update")
                   pkg)
        (message "Aborted"))
    (message "%s is up-to-date" package)))
