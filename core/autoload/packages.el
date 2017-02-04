;;; packages.el

(defvar doom-packages-last-refresh nil
  "A timestamp indicating the last time `package-refresh-contents' was run.")

;;;###autoload
(defun doom-refresh-packages ()
  "Refresh ELPA packages."
  (when (or (not doom-packages-last-refresh)
            (> (nth 1 (time-since doom-packages-last-refresh)) 3600))
    (doom-initialize)
    (package-refresh-contents)
    (setq doom-packages-last-refresh (current-time))))

;;;###autoload
(defun doom-package-elpa-p (name)
  "Returns non-nil if NAME was a package installed with elpa."
  (doom-initialize)
  (and (assq name package-alist)
       (not (doom-package-quelpa-p name))))

;;;###autoload
(defun doom-package-quelpa-p (name)
  "Returns non-nil if NAME was a package installed with quelpa."
  (unless (quelpa-setup-p)
    (error "Could not initialize quelpa"))
  (assq name quelpa-cache))

;;;###autoload
(defun doom-package-outdated-p (name)
  "Determine whether NAME (a symbol) is outdated or not. If outdated, returns a
list, whose car is NAME, and cdr the current version list and latest version
list of the package."
  (doom-refresh-packages)
  (package-read-all-archive-contents)
  (when (assq name package-alist)
    (let* ((old-version
            (package-desc-version (cadr (or (assq name package-alist)
                                            (assq name package--builtins)))))
           (new-version
            (cond ((doom-package-quelpa-p name)
                   (let ((recipe (assq name quelpa-cache))
                         (dir (f-expand (symbol-name name) quelpa-build-dir))
                         (inhibit-message t))
                     (or (quelpa-checkout recipe dir)
                         old-version)))

                  ((doom-package-elpa-p name)
                   (package-desc-version (cadr (assq name package-archive-contents)))))))
      (unless (version-list-<= new-version old-version)
        (cons name old-version new-version)))))

;;;###autoload
(defun doom-get-packages (&optional backend)
  "Retrieves a list of explicitly installed packages (i.e. non-dependencies).
Each element is a cons cell, whose car is the package symbol and whose cdr is
the quelpa recipe (if any).

BACKEND can be 'quelpa or 'elpa, and will instruct this function to return only
the packages relevant to that backend."
  (doom-reload)
  (unless (quelpa-setup-p)
    (error "Could not initialize quelpa"))
  (--map (cons it (assq it quelpa-cache))
         (-intersection (package--find-non-dependencies)
                        (append (mapcar 'car doom-packages) doom-protected-packages))))

;;;###autoload
(defun doom-get-outdated-packages ()
  "Return a list of packages that are out of date. Each element is a sublist,
containing (list package-symbol current-version-string new-version-string). Can
be fed to `doom/packages-update'."
  (-non-nil (--map (doom-package-outdated-p (car it)) (doom-get-packages))))

;;;###autoload
(defun doom-get-orphaned-packages ()
  "Return a list of packages that are no longer needed or depended on. Can be
fed to `doom/packages-delete'."
  (doom-reload)
  (let ((package-selected-packages
         (append (mapcar 'car doom-packages) doom-protected-packages)))
    (package--removable-packages)))

;;;###autoload
(defun doom-get-packages-to-install ()
  "Return a list of packages that aren't installed, but need to be. Used by
`doom/packages-install'."
  (doom-reload)
  (--remove (assq (car it) package-alist)
            (append doom-packages (-map 'list doom-protected-packages))))

;;;###autoload
(defun doom*package-delete (name)
  "Makes `package-delete' update `quelpa-cache'."
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

(defun doom-install-package (name &optional recipe)
  "Installs package NAME with optional quelpa RECIPE (see `quelpa-recipe' for an
example; the package name can be omitted)."
  (doom-refresh-packages)
  (when (package-installed-p name)
    (error "%s is already installed" name))
  (cond (recipe (quelpa (plist-get plist :recipe)))
        (t (package-install name)))
  (add-to-list 'doom-packages (cons name recipe))
  (package-installed-p name))

(defun doom-update-package (name)
  "Updates package NAME if it is out of date, using quelpa or package.el as
appropriate."
  (doom-refresh-packages)
  (unless (package-installed-p name)
    (error "%s isn't installed" name))
  (when (doom-package-outdated-p name)
    (let (quelpa-modified-p)
      (cond ((doom-package-quelpa-p name)
             (let ((quelpa-upgrade-p t))
               (quelpa it)
               (setq quelpa-modified-p t)))
            (t
             (let ((desc    (cadr (assq name package-alist)))
                   (archive (cadr (assq name package-archive-contents))))
               (package-install-from-archive archive)
               (delete-directory (package-desc-dir desc) t))
             (package-install name))))
    (when quelpa-modified-p
      (quelpa-save-cache))
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
  (let ((packages (doom-get-packages-to-install)))
    (cond ((not packages)
           (message "No packages to install!"))

          ((not (y-or-n-p
                 (format "%s packages will be installed:\n%s\n\nProceed?"
                         (length packages)
                         (mapconcat (lambda (pkg) (format "+ %s (%s)"
                                                     (symbol-name (car pkg))
                                                     (cond ((cdr pkg) "QUELPA")
                                                           (t "ELPA"))))
                                    packages "\n"))))
           (message "Aborted!"))

          (t
           (doom-message "Installing %s packages" (length packages))

           (dolist (pkg packages)
             (condition-case ex
                 (doom-message "%s %s (%s)"
                               (let ((plist (cdr pkg)))
                                 (if (doom-install-package (car pkg) (cdr pkg))
                                     "Installed"
                                   "Failed to install"))
                               pkg
                               (cond ((cdr pkg) "QUELPA")
                                     (t "ELPA")))
               (error
                (doom-message "Error installing %s: %s" (car pkg) ex))))

           (doom-message "Finished!")))))

;;;###autoload
(defun doom/packages-update ()
  "Interactive command for updating packages."
  (interactive)
  (let ((packages (doom-get-outdated-packages)))
    (cond ((not packages)
           (message "Everything is up-to-date"))

          ((not (y-or-n-p
                 (format "%s packages will be updated:\n%s\n\nProceed?"
                         (length packages)
                         (mapconcat (lambda (pkg) (format "%s: %s -> %s"
                                                     (car pkg)
                                                     (car (cdr pkg))
                                                     (cdr (cdr pkg))))
                                    (--sort (string-lessp (symbol-name (car it))
                                                          (symbol-name (car other)))
                                            outdated-packages) ", "))))
           (message "Aborted!"))

          (t
           (dolist (pkg packages)
             (condition-case ex
                 (doom-message "%s %s"
                               (if (doom-update-package pkg)
                                   "Updated"
                                 "Failed to update")
                               pkg)
               (error
                (doom-message "Error installing %s: %s" pkg ex))))

           (doom-message "Finished!")))))

;;;###autoload
(defun doom/packages-autoremove ()
  "Interactive command for auto-removing orphaned packages."
  (interactive)
  (let ((packages (doom-get-orphaned-packages)))
    (cond ((not packages)
           (message "No unused packages to remove"))

          ((not (y-or-n-p
                 (format "%s packages will be deleted:\n%s\n\nProceed?"
                         (length packages)
                         (mapconcat 'symbol-name (-sort 'string-lessp packages) ", "))))
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

;;;###autoload
(defalias 'doom/package-install 'package-install)

;;;###autoload
(defun doom/package-delete (&optional package)
  (interactive
   (list (completing-read "Delete package: " (doom-get-packages))))
  (if (package-installed-p package)
      (message "%s %s"
               (if (doom-delete-package package)
                   "Deleted"
                 "Failed to delete")
               pkg)
    (message "%s isn't installed" package)))

;;;###autoload
(defun doom/package-update (&optional package)
  (interactive
   (list (completing-read "Update package: " (doom-get-packages))))
  (if (doom-package-outdated-p package)
      (message "%s %s"
               (if (doom-update-package package)
                   "Updated"
                 "Failed to update")
               pkg)
    (message "%s is up-to-date" package)))
