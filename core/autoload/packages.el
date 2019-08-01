;;; core/autoload/packages.el -*- lexical-binding: t; -*-

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
  (when-let* ((plist (doom-package-get package)))
    (not (eval (plist-get plist :ignore) t))))

;;;###autoload
(defun doom-package-private-p (package)
  "Return non-nil if PACKAGE was installed by the user's private config."
  (assq :private (doom-package-get package :modules)))

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
(defun doom-package-backend (package)
  "Return 'straight, 'builtin, 'elpa or 'other, depending on how PACKAGE is
installed."
  (cond ((gethash (symbol-name package) straight--build-cache)
         'straight)
        ((or (doom-package-built-in-p package)
             (assq package package--builtins))
         'builtin)
        ((assq package package-alist)
         'elpa)
        ('other)))

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
                (unless (let ((ppss (syntax-ppss)))
                          (or (nth 3 ppss)
                              (nth 4 ppss)))
                  (cl-destructuring-bind (name . plist)
                      (cdr (sexp-at-point))
                    (push (cons
                           name (plist-put
                                 plist :modules
                                 (list (doom-module-from-path file))))
                          doom-packages)))))))
      ((debug error)
       (signal 'doom-package-error
               (list (doom-module-from-path file)
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
     (doom-path doom-core-dir "packages.el") all-p t)
    (let ((private-packages (doom-path doom-private-dir "packages.el")))
      (unless all-p
        ;; We load the private packages file twice to ensure disabled packages
        ;; are seen ASAP, and a second time to ensure privately overridden
        ;; packages are properly overwritten.
        (doom--read-module-packages-file private-packages t t))
      (if all-p
          (mapc #'doom--read-module-packages-file
                (doom-files-in doom-modules-dir
                               :depth 2
                               :match "/packages\\.el$"))
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
