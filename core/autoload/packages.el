;;; core/autoload/packages.el -*- lexical-binding: t; -*-

(require 'core-packages)


;;
;;; Package metadata

;;;###autoload
(defun doom-package-get (package &optional prop nil-value)
  "Returns PACKAGE's `package!' recipe from `doom-packages'."
  (let ((plist (cdr (assq package doom-packages))))
    (if prop
        (if (plist-member plist prop)
            (plist-get plist prop)
          nil-value)
      plist)))

;;;###autoload
(defun doom-package-recipe (package &optional prop nil-value)
  "Returns the `straight' recipe PACKAGE was registered with."
  (let ((plist (gethash (symbol-name package) straight--recipe-cache)))
    (if prop
        (if (plist-member plist prop)
            (plist-get plist prop)
          nil-value)
      plist)))

;;;###autoload
(defun doom-package-build-recipe (package &optional prop nil-value)
  "Returns the `straight' recipe PACKAGE was installed with."
  (let ((plist (nth 2 (gethash (symbol-name package) straight--build-cache))))
    (if prop
        (if (plist-member plist prop)
            (plist-get plist prop)
          nil-value)
      plist)))

;;;###autoload
(defun doom-package-build-time (package)
  "TODO"
  (car (gethash (symbol-name package) straight--build-cache)))

;;;###autoload
(defun doom-package-dependencies (package &optional recursive noerror)
  "Return a list of dependencies for a package."
  (let ((deps (nth 1 (gethash (symbol-name package) straight--build-cache))))
    (if recursive
        (nconc deps (mapcan (lambda (dep) (doom-package-dependencies dep t t))
                            deps))
      deps)))

(defun doom-package-depending-on (package &optional noerror)
  "Return a list of packages that depend on the package named NAME."
  (cl-check-type name symbol)
  ;; can't get dependencies for built-in packages
  (unless (or (doom-package-build-recipe name)
              noerror)
    (error "Couldn't find %s, is it installed?" name))
  (cl-loop for pkg in (hash-table-keys straight--build-cache)
           for deps = (doom-package-dependencies pkg)
           if (memq package deps)
           collect pkg
           and append (doom-package-depending-on pkg t)))


;;
;;; Predicate functions

;;;###autoload
(defun doom-package-built-in-p (package)
  "Return non-nil if PACKAGE (a symbol) is built-in."
  (eq (doom-package-build-recipe package :type)
      'built-in))

;;;###autoload
(defun doom-package-installed-p (package)
  "Return non-nil if PACKAGE (a symbol) is installed."
  (file-directory-p (straight--build-dir (symbol-name package))))

;;;###autoload
(defun doom-package-registered-p (package)
  "Return non-nil if PACKAGE (a symbol) has been registered with `package!'.

Excludes packages that have a non-nil :built-in property."
  (when-let (plist (doom-package-get package))
    (not (eval (plist-get plist :ignore) t))))

;;;###autoload
(defun doom-package-private-p (package)
  "Return non-nil if PACKAGE was installed by the user's private config."
  (doom-package-get package :private))

;;;###autoload
(defun doom-package-protected-p (package)
  "Return non-nil if PACKAGE is protected.

A protected package cannot be deleted and will be auto-installed if missing."
  (memq package doom-core-packages))

;;;###autoload
(defun doom-package-core-p (package)
  "Return non-nil if PACKAGE is a core Doom package."
  (or (doom-package-protected-p package)
      (assq :core (doom-package-get package :modules))))

;;;###autoload
(defun doom-package-different-recipe-p (name)
  "Return t if a package named NAME (a symbol) has a different recipe than it
was installed with."
  (cl-check-type name symbol)
  ;; TODO
  ;; (when (doom-package-installed-p name)
  ;;   (when-let* ((doom-recipe (assq name doom-packages))
  ;;               (install-recipe (doom-package-recipe)))
  ;;     (not (equal (cdr quelpa-recipe)
  ;;                 (cdr (plist-get (cdr doom-recipe) :recipe))))))
  )


;;
;;; Package list getters

;;;###autoload
(cl-defun doom-find-packages (&key (installed 'any)
                                   (private 'any)
                                   (disabled 'any)
                                   (pinned 'any)
                                   (ignored 'any)
                                   (core 'any)
                                   _changed
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

Warning: this function is expensive, as it re-evaluates your all packages.el
files."
  (delete-dups
   (cl-loop for (sym . plist) in doom-packages
            if (and (or (not backend)
                        (eq (doom-package-backend sym 'noerror) backend))
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
            and if (and deps (not (doom-package-built-in-p sym)))
            nconc
            (cl-loop for pkg in (doom-package-dependencies sym 'recursive 'noerror)
                     if (or (eq installed 'any)
                            (if installed
                                (doom-package-installed-p pkg)
                              (not (doom-package-installed-p pkg))))
                     collect (cons pkg (cdr (assq pkg doom-packages)))))))

(defun doom--read-module-packages-file (file &optional eval noerror)
  (with-temp-buffer ; prevent buffer-local settings from propagating
    (condition-case e
        (if (not eval)
            (load file noerror t t)
          (when (file-readable-p file)
            (insert-file-contents file)
            (delay-mode-hooks (emacs-lisp-mode))
            (while (re-search-forward "(package! " nil t)
              (save-excursion
                (goto-char (match-beginning 0))
                (unless (string-match-p
                         "^.*;" (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (point)))
                  (cl-destructuring-bind (name . plist) (cdr (sexp-at-point))
                    (push (cons name
                                (plist-put plist :modules
                                           (cond ((file-in-directory-p file doom-private-dir)
                                                  '((:private)))
                                                 ((file-in-directory-p file doom-core-dir)
                                                  '((:core)))
                                                 ((doom-module-from-path file)))))
                          doom-packages)))))))
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
        (doom-modules (doom-modules))
        doom-packages
        doom-disabled-packages)
    (doom--read-module-packages-file
     (expand-file-name "packages.el" doom-core-dir)
     all-p t)
    (let ((private-packages (expand-file-name "packages.el" doom-private-dir)))
      (unless all-p
        ;; We load the private packages file twice to ensure disabled packages
        ;; are seen ASAP, and a second time to ensure privately overridden
        ;; packages are properly overwritten.
        (doom--read-module-packages-file private-packages t t))
      (if all-p
          (mapc #'doom--read-module-packages-file
                (doom-files-in doom-modules-dir
                               :depth 2
                               :full t
                               :match "/packages\\.el$"
                               :sort nil))
        (cl-loop for key being the hash-keys of doom-modules
                 for path = (doom-module-path (car key) (cdr key) "packages.el")
                 for doom--current-module = key
                 do (doom--read-module-packages-file path nil t)))
      (doom--read-module-packages-file private-packages all-p t))
    (nreverse doom-packages)))


;;
;;; Main functions

;;;###autoload
(defun doom/reload-packages ()
  "Reload `doom-packages', `package' and `quelpa'."
  (interactive)
  (message "Reloading packages")
  (doom-initialize-packages t)
  (message "Reloading packages...DONE"))
