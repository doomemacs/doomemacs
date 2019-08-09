;; -*- no-byte-compile: t; -*-
;;; core/cli/packages.el

(dispatcher! (install i) (doom--do #'doom-packages-install)
  "Installs requested packages that aren't installed.")

(dispatcher! (update u) (doom--do #'doom-packages-update)
  "Updates packages.")

(dispatcher! (autoremove r) (doom--do #'doom-packages-autoremove)
  "Removes packages that are no longer needed.")


;;
;; Helpers

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

(defsubst doom--do (fn)
  (doom-reload-doom-autoloads)
  (when (funcall fn doom-auto-accept)
    (doom-reload-package-autoloads)))


;;
;; Library

(defun doom-packages-install (&optional auto-accept-p)
  "Interactive command for installing missing packages."
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
  "Interactive command for updating packages."
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
  "Interactive command for auto-removing orphaned packages."
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
