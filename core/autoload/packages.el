;;; packages.el

;;;###autoload
(defun doom-package-outdated-p (package)
  "Determine whether PACKAGE (a symbol) is outdated or not. If outdated, returns
a cons cell, whose car is the current version string of PACKAGE (a symbol), and
whose cdr is the latest version of the package. Be sure to run
`package-refresh-contents' beforehand, or the return value could be out of
date."
  (unless package-selected-packages
    (doom-initialize))
  (when (and (memq package package-selected-packages)
             (package-installed-p package)
             (quelpa-setup-p))
    (let* ((pkg-recipe (cdr (assq 'quelpa quelpa-cache)))
           (cur-desc (cadr (or (assq package package-alist)
                               (assq package package--builtins))))
           (cur-version (package-desc-version cur-desc))
           (inhibit-message t)
           new-version)
      (setq new-version
            (if pkg-recipe
                (let ((ver (quelpa-checkout
                            pkg-recipe
                            (f-expand (symbol-name package) quelpa-build-dir))))
                  (or (and ver (version-to-list ver)) cur-version))
              (package-desc-version (cadr (assq package package-archive-contents)))))
      (unless (version-list-<= new-version cur-version)
        (cons cur-version new-version)))))

;;;###autoload
(defun doom/packages-install ()
  "Install missing packages."
  (interactive)
  (let ((pkg-n (doom-reload-packages :install)))
    (if (= pkg-n 0)
        (message "Nothing to install")
      (message "\nInstalled %s packages:\n%s" pkg-n
               (mapconcat (lambda (pkg) (concat "+ " (symbol-name pkg)))
                          doom-installed-packages "\n")))))

;;;###autoload
(defun doom/packages-update ()
  "Update outdated packages. This includes quelpa-installed packages and ELPA
packages. This will delete old versions of packages as well."
  (interactive)
  (message "Refreshing packages...")
  (doom-initialize t)
  (if (not package-alist)
      (message "No packages are installed")
    (require 'quelpa)
    (when (quelpa-setup-p)
      (setq quelpa-cache (--filter (package-installed-p (car it)) quelpa-cache)))
    (let* ((err 0)
           (quelpa-packages (-map 'car quelpa-cache))
           (elpa-packages (-difference (package--find-non-dependencies) quelpa-packages))
           quelpa-modified-p
           outdated-packages)
      (message "ELPA\n%s\n\nQUELPA\n%s" elpa-packages quelpa-packages)
      (dolist (pkg (append quelpa-packages elpa-packages))
        (awhen (doom-package-outdated-p pkg)
          (push (list pkg it) outdated-packages)))
      (message "\nOUTDATED\n%s" outdated-packages)
      ;; (cond ((not outdated-packages)
      ;;        (message "Everything is up-to-date"))

      ;;       ((not (y-or-n-p
      ;;              (format "%s packages will be updated:\n%s\n\nProceed?"
      ;;                      (length outdated-packages)
      ;;                      (mapconcat (lambda (pkg) (format "%s: %s -> %s"
      ;;                                                  (car pkg)
      ;;                                                  (car (cdr pkg))
      ;;                                                  (cdr (cdr pkg))))
      ;;                                 (--sort (string-lessp (symbol-name (car it))
      ;;                                                       (symbol-name (car other)))
      ;;                                         outdated-packages) ", "))))
      ;;        (message "Aborted"))

      ;;       (t
      ;;        (dolist (pkg outdated-packages)
      ;;          (condition-case ex
      ;;              (cond ((assq pkg quelpa-outdated-packages)
      ;;                     (let ((inhibit-message t))
      ;;                       (quelpa package)
      ;;                       (setq quelpa-modified-p t)))
      ;;                    ((memq pkg elpa-outdated-packages)
      ;;                     (let ((desc (cadr (assq pkg package-alist)))
      ;;                           (archive (cadr (assoc pkg package-archive-contents))))
      ;;                       (package-install-from-archive archive)
      ;;                       (delete-directory (package-desc-dir desc) t)))
      ;;                    (t (error "Not a valid package")))
      ;;            ('error
      ;;             (setq err (1+ err))
      ;;             (message "ERROR (%s): %s" pkg ex))))))
      ;; (when quelpa-modified-p
      ;;   (quelpa-save-cache))
      ;; (if (> err 0)
      ;;     (message "Done, but with %s errors" err)
      ;;   (message "Done"))
      )))

;;;###autoload
(defun doom/packages-clean ()
  "Delete packages that are no longer used or depended on."
  (interactive)
  (doom-reload-packages)
  (let* ((package-selected-packages (-intersection (package--find-non-dependencies)
                                                   (append doom-packages doom-protected-packages)))
         (packages-to-delete (-difference (package--removable-packages) doom-protected-packages))
         quelpa-modified-p)
    (cond ((not package-selected-packages)
           (message "No packages installed!"))

          ((not packages-to-delete)
           (message "No unused packages to remove."))

          ((not (y-or-n-p
                 (format "%s packages will be deleted:\n%s\n\nProceed?"
                         (length packages-to-delete)
                         (mapconcat 'symbol-name (-sort 'string-lessp packages-to-delete) ", "))))
           (message "Aborted."))

          (t
           (require 'quelpa)
           (quelpa-setup-p)
           (dolist (p packages-to-delete)
             (package-delete (cadr (assq p package-alist)) t)
             (when (and quelpa-cache (assq p quelpa-cache))
               (setq quelpa-cache (assq-delete-all p quelpa-cache)
                     quelpa-modified-p t)))
           (when quelpa-modified-p
             (quelpa-save-cache))))))

;;;###autoload
(defun doom/packages-reload ()
  "Reload `load-path' by scanning all packages. Run this if you ran make update
or make clean outside of Emacs."
  (interactive)
  (doom-initialize t)
  (message "Reloaded %s packages" (length package-alist)))

;;;###autoload
(defun doom/packages-delete (&optional package)
  "Attempt to delete PACKAGE. Wraps around `package-delete'."
  (interactive)
  (doom-reload-packages)
  (let* ((pkg (or package (completing-read "Delete package: " doom-packages nil t)))
         (pkg-desc (cdr (assq pkg package-alist))))
    (unless pkg-desc
      (error "Couldn't find the package %s" package))
    (package-delete pkg-desc)))
