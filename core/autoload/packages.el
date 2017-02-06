;;; packages.el
(provide 'doom-lib-packages)

(defvar doom-packages-last-refresh nil
  "A timestamp indicating the last time `package-refresh-contents' was run.")

;;;###autoload
(defun doom-refresh-packages ()
  "Refresh ELPA packages."
  (doom-initialize)
  (when (or (not doom-packages-last-refresh)
            (> (nth 1 (time-since doom-packages-last-refresh)) 3600))
    (package-refresh-contents)
    (setq doom-packages-last-refresh (current-time))))

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
  (let ((pkg (or (assq name package-alist)
                 (assq name package--builtins))))
    (when pkg
      (let* ((old-version (package-desc-version (cadr pkg)))
             (new-version
              (pcase (doom-package-backend name)
                ('quelpa
                 (let ((recipe (assq name quelpa-cache))
                       (dir (f-expand (symbol-name name) quelpa-build-dir))
                       (inhibit-message t))
                   (or (and (quelpa-setup-p) (quelpa-checkout recipe dir))
                       old-version)))
                ('elpa
                 (package-desc-version (cadr (assq name package-archive-contents)))))))
        (when (stringp new-version)
          (setq new-version (version-to-list new-version)))
        (when (version-list-< old-version new-version)
          (list name old-version new-version))))))

;;;###autoload
(defun doom-get-packages (&optional backend)
  "Retrieves a list of explicitly installed packages (i.e. non-dependencies).
Each element is a cons cell, whose car is the package symbol and whose cdr is
the quelpa recipe (if any).

BACKEND can be 'quelpa or 'elpa, and will instruct this function to return only
the packages relevant to that backend."
  (doom-read-packages t)
  (unless (quelpa-setup-p)
    (error "Could not initialize quelpa"))
  (-non-nil
   (--map (or (assq it doom-packages)
              (list (car (assq it package-alist))))
          (append (mapcar 'car doom-packages)
                  doom-protected-packages))))

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
  (doom-read-packages t)
  (let ((package-selected-packages (append (mapcar 'car doom-packages) doom-protected-packages)))
    (package--removable-packages)))

;;;###autoload
(defun doom-get-packages-to-install ()
  "Return a list of packages that aren't installed, but need to be. Used by
`doom/packages-install'."
  (doom-read-packages t)
  (--remove (assq (car it) package-alist)
            (append doom-packages (-map 'list doom-protected-packages))))

(defun doom--scrape-sexps (sym file)
  (declare (indent defun))
  (unless (f-exists-p file)
    (error "%s does not exist" file))
  (unless (symbolp sym)
    (error "%s is not a valid symbol" sym))
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((regexp (concat "\\(^\\|\\s-\\)(" (symbol-name sym) " "))
          sexps)
      (while (re-search-forward regexp nil t)
        (let ((sexp (cdr-safe (save-excursion
                                (beginning-of-defun)
                                (sexp-at-point)))))
          (push sexp sexps)))
      (reverse sexps))))

;;;###autoload
(defun doom-read-packages (&optional force-p nopackages)
  "Parses your Emacs config to keep track of packages declared with `package!'
in `doom-packages' and enabled modules in `doom-enabled-modules'."
  (doom-initialize)
  (when (or force-p (not doom-enabled-modules) (not doom-packages))
    (setq doom-enabled-modules
          (let (paths mode enabled-modules)
            (--each (doom--scrape-sexps 'doom! (f-expand "init.el" doom-emacs-dir))
              (dolist (module it)
                (cond ((keywordp module)
                       (setq mode module))
                      ((not mode)
                       (error "Malformed doom! call: no namespace for %s" module))
                      (t
                       (push (cons mode module) enabled-modules)))))
            enabled-modules))

    (unless nopackages
      (setq package-pinned-packages nil
            doom-packages nil)
      (mapc (lambda (pkg) (cl-pushnew pkg doom-packages :key 'car))
            (-map (lambda (args)
                    (plist! args &delete
                      :preface :ensure :requires :no-require :bind :bind* :bind-keymap
                      :bind-keymap* :interpreter :mode :commands :defines :functions
                      :defer :init :after :demand :config :diminish :delight))
                  (--sort (string-greaterp (symbol-name (car it))
                                           (symbol-name (car other)))
                          (-flatten-n
                           1 (mapcar (lambda (file)
                                       (when (f-exists-p file)
                                         (doom--scrape-sexps 'package! file)))
                                     (append (f-glob "core*.el" doom-core-dir)
                                             (--map (doom-module-path (car it) (cdr it) "packages.el")
                                                    doom-enabled-modules)))))))
      t)))

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
  (when (package-installed-p name)
    (error "%s is already installed, skipping" name))
  (when (plist-get plist :disabled)
    (error "%s is disabled, skipping" name))
  (when (plist-get plist :load-path)
    (error "%s has a local load-path, skipping" name))
  (cond ((plist-get plist :recipe)
         (let ((recipe (plist-get plist :recipe)))
           (when (and recipe (= 0 (mod (length recipe) 2)))
             (setq recipe (cons name recipe)))
           (quelpa recipe)))
        (t (package-install name)))
  (cl-pushnew (cons name plist) doom-packages :key 'car)
  (when (plist-member plist :setup)
    (let ((setup (plist-get plist :setup)))
      (when (listp setup)
        (setq setup (assq (doom-os) setup)))
      (when setup
        (async-shell-command setup))))
  (package-installed-p name))

(defun doom-update-package (name)
  "Updates package NAME if it is out of date, using quelpa or package.el as
appropriate."
  (doom-refresh-packages)
  (unless (package-installed-p name)
    (error "%s isn't installed" name))
  (when (doom-package-outdated-p name)
    (let (quelpa-modified-p)
      (pcase (doom-package-backend name)
        ('quelpa
         (let ((quelpa-upgrade-p t))
           (quelpa it)
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
  (let ((packages (doom-get-packages-to-install)))
    (cond ((not packages)
           (message "No packages to install!"))

          ((not (y-or-n-p
                 (format "%s packages will be installed:\n\n%s\n\nProceed?"
                         (length packages)
                         (mapconcat (lambda (pkg)
                                      (format "+ %s (%s)"
                                              (car pkg)
                                              (cond ((plist-get (cdr pkg) :recipe) "QUELPA")
                                                    (t "ELPA"))))
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
                               (if (doom-install-package (car pkg) (cdr pkg))
                                   "Installed"
                                 "Failed to install")
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
                 (format "%s packages will be updated:\n\n%s\n\nProceed?"
                         (length packages)
                         (mapconcat
                          (lambda (pkg) (format "+ %s %s -> %s\t%s"
                                           (s-pad-right 20 " " (symbol-name (car pkg)))
                                           (car (cdr pkg))
                                           (car (cdr (cdr pkg)))))
                          (--sort (string-lessp (symbol-name (car it))
                                                (symbol-name (car other)))
                                  packages)
                          "\n"))))
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

;;;###autoload
(defalias 'doom/install-package 'package-install)

;;;###autoload
(defun doom/delete-package (&optional package)
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
(defun doom/update-package (&optional package)
  "Use this instead of package.el's update interface."
  (declare (interactive-only t))
  (interactive
   (list (completing-read "Update package: " (doom-get-packages))))
  (if (doom-package-outdated-p package)
      (message "%s %s"
               (if (doom-update-package package)
                   "Updated"
                 "Failed to update")
               pkg)
    (message "%s is up-to-date" package)))
