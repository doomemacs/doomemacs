;; -*- no-byte-compile: t; -*-
;;; core/cli/packages.el

;;
;;; Helpers

(defmacro doom--condition-case! (&rest body)
  `(condition-case-unless-debug e
       (progn ,@body)
     ('user-error
      (print! (bold (red "  NOTICE: %s")) e))
     ('file-error
      (print! "  %s\n  %s"
              (bold (red "FILE ERROR: %s" (error-message-string e)))
              "Trying again...")
      (quiet! (doom-refresh-packages-maybe t))
      ,@body)
     ('error
      (print! (bold (red "  %s %s\n  %s"))
              "FATAL ERROR: " e
              "Run again with the -d flag for details"))))

(defsubst doom--ensure-autoloads-while (fn)
  (doom-reload-doom-autoloads)
  (when (funcall fn doom-auto-accept)
    (doom-reload-package-autoloads)))


;;
;;; Dispatchers

(dispatcher! (install i)
  (doom--ensure-autoloads-while #'doom-packages-install)
  "Installs wanted packages that aren't installed.

Package management in Doom is declarative. A `package!' declaration in an
enabled module or your private packages.el marks a package as 'wanted'.")

(dispatcher! (update u)
  (doom--ensure-autoloads-while #'doom-packages-update)
  "Updates packages.

This excludes packages whose `package!' declaration contains a non-nil :freeze
or :ignore property.")

(dispatcher! (autoremove r)
  (doom--ensure-autoloads-while #'doom-packages-autoremove)
  "Removes packages that are no longer needed.

This includes packages installed with 'M-x package-install' without an
accompanying `package!' declaration in an enabled module's packages.el file or
your private one.")


;;
;;; Library

(defun doom-packages-install (&optional auto-accept-p)
  "Installs missing packages.

This function will install any primary package (i.e. a package with a `package!'
declaration) or dependency thereof that hasn't already been.

Unless AUTO-ACCEPT-P is non-nil, this function will prompt for confirmation with
a list of packages that will be installed."
  (print! "Looking for packages to install...")
  (let ((packages (doom-get-missing-packages)))
    (cond ((not packages)
           (print! (green "No packages to install!"))
           nil)

          ((not (or auto-accept-p
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
                                               (format "%s -> %s"
                                                       (doom-package-backend (car pkg) 'noerror)
                                                       (doom-package-recipe-backend (car pkg) 'noerror)))
                                              ((plist-get (cdr pkg) :recipe)
                                               "quelpa")
                                              ("elpa"))))
                              (cl-sort (cl-copy-list packages) #'string-lessp
                                       :key #'car)
                              "\n")))))
           (user-error "Aborted!"))

          ((let (success)
             (doom-refresh-packages-maybe doom-debug-mode)
             (dolist (pkg packages)
               (print! "Installing %s" (car pkg))
               (doom--condition-case!
                (let ((result
                       (or (and (doom-package-installed-p (car pkg))
                                (not (doom-package-different-backend-p (car pkg)))
                                (not (doom-package-different-recipe-p (car pkg)))
                                'already-installed)
                           (and (doom-install-package (car pkg) (cdr pkg))
                                (setq success t)
                                'success)
                           'failure))
                      (pin-label
                       (and (plist-member (cdr pkg) :pin)
                            (format " [pinned: %s]" (plist-get (cdr pkg) :pin)))))
                  (print! "%s%s"
                          (pcase result
                            (`already-installed (dark (white "⚠ ALREADY INSTALLED")))
                            (`success (green "✓ DONE"))
                            (`failure (red "✕ FAILED")))
                          (or pin-label "")))))
             (print! (bold (green "Finished!")))
             (when success
               (set-file-times doom-packages-dir)
               (doom-delete-autoloads-file doom-package-autoload-file))
             success)))))

(defun doom-packages-update (&optional auto-accept-p)
  "Updates packages.

Unless AUTO-ACCEPT-P is non-nil, this function will prompt for confirmation with
a list of packages that will be updated."
  (print! "Looking for outdated packages...")
  (let ((packages (cl-sort (cl-copy-list (doom-get-outdated-packages)) #'string-lessp
                           :key #'car)))
    (cond ((not packages)
           (print! (green "Everything is up-to-date"))
           nil)

          ((not (or auto-accept-p
                    (y-or-n-p
                     (format "%s packages will be updated:\n\n%s\n\nProceed?"
                             (length packages)
                             (let ((max-len
                                    (or (car (sort (mapcar (lambda (it) (length (symbol-name (car it)))) packages)
                                                   #'>))
                                        10)))
                               (mapconcat
                                (lambda (pkg)
                                  (format (format "+ %%-%ds (%%s) %%-%ds -> %%s"
                                                  (+ max-len 2) 14)
                                          (symbol-name (car pkg))
                                          (doom-package-backend (car pkg))
                                          (package-version-join (cadr pkg))
                                          (package-version-join (cl-caddr pkg))))
                                packages
                                "\n"))))))
           (user-error "Aborted!"))

          ((let (success)
             (dolist (pkg packages)
               (print! "Updating %s" (car pkg))
               (doom--condition-case!
                (print!
                 (let ((result (doom-update-package (car pkg) t)))
                   (when result (setq success t))
                   (color (if result 'green 'red)
                          (if result "✓ DONE" "✕ FAILED"))))))
             (print! (bold (green "Finished!")))
             (when success
               (set-file-times doom-packages-dir)
               (doom-delete-autoloads-file doom-package-autoload-file))
             success)))))

(defun doom-packages-autoremove (&optional auto-accept-p)
  "Auto-removes orphaned packages.

An orphaned package is a package that isn't a primary package (i.e. doesn't have
a `package!' declaration) or isn't depended on by another primary package.

Unless AUTO-ACCEPT-P is non-nil, this function will prompt for confirmation with
a list of packages that will be removed."
  (print! "Looking for orphaned packages...")
  (let ((packages (doom-get-orphaned-packages)))
    (cond ((not packages)
           (print! (green "No unused packages to remove"))
           nil)

          ((not
            (or auto-accept-p
                (y-or-n-p
                 (format "%s packages will be deleted:\n\n%s\n\nProceed?"
                         (length packages)
                         (mapconcat
                          (lambda (sym)
                            (let ((old-backend (doom-package-backend sym 'noerror))
                                  (new-backend (doom-package-recipe-backend sym 'noerror)))
                              (format "+ %s (%s)" sym
                                      (cond ((null new-backend)
                                             "removed")
                                            ((eq old-backend new-backend)
                                             (symbol-name new-backend))
                                            ((format "%s -> %s" old-backend new-backend))))))
                          (sort (cl-copy-list packages) #'string-lessp)
                          "\n")))))
           (user-error "Aborted!"))

          ((let (success)
             (dolist (pkg packages)
               (doom--condition-case!
                (let ((result (doom-delete-package pkg t)))
                  (if result (setq success t))
                  (print! (color (if result 'green 'red) "%s %s")
                          (if result "✓ Removed" "✕ Failed to remove")
                          pkg))))
             (print! (bold (green "Finished!")))
             (when success
               (set-file-times doom-packages-dir)
               (doom-delete-autoloads-file doom-package-autoload-file))
             success)))))
