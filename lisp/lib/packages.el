;;; lisp/lib/packages.el -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; Emacs package management is opinionated, and so is Doom. Doom uses `straight'
;; to create a declarative, lazy-loaded, and (nominally) reproducible package
;; management system. We use `straight' over `package' because the latter is
;; tempermental. ELPA sources suffer downtime occasionally and often fail to
;; build packages when GNU Tar is unavailable (e.g. MacOS users start with BSD
;; tar). Known gnutls errors plague the current stable release of Emacs (26.x)
;; which bork TLS handshakes with ELPA repos (mainly gnu.elpa.org). See
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=3434.
;;
;; What's worse, you can only get the latest version of packages through ELPA.
;; In an ecosystem that is constantly changing, this is more frustrating than
;; convenient. Straight (and Doom) can do rolling release, but it is opt-in.
;;
;; Interacting with this package management system is done through Doom's
;; bin/doom script. Find out more about it by running 'doom help' (I highly
;; recommend you add the script to your PATH). Here are some highlights:
;;
;; - `doom install`: a wizard that guides you through setting up Doom and your
;;   private config for the first time.
;; - `doom sync`: your go-to command for making sure Doom is in optimal
;;   condition. It ensures all unneeded packages are removed, all needed ones
;;   are installed, and all metadata associated with them is generated.
;; - `doom upgrade`: upgrades Doom Emacs and your packages to the latest
;;   versions. There's also 'bin/doom sync -u' for updating only your packages.
;;
;; How this works is: the system reads packages.el files located in each
;; activated module, your private config (`doom-user-dir'), and one in
;; `doom-core-dir'. These contain `package!' declarations that tell DOOM what
;; packages to install and where from.
;;
;; All that said, you can still use package.el's commands, but 'doom sync' will
;; purge ELPA packages.
;;
;;; Code:

(require 'doom-straight)
(doom-require 'doom-lib 'modules)


;;
;;; Variables

;; DEPRECATED: Will be stored in the local profile in v3.0
(defvar doom-packages ()
  "A list of enabled packages. Each element is a sublist, whose CAR is the
package's name as a symbol, and whose CDR is the plist supplied to its
`package!' declaration. Set by `doom-initialize-packages'.")

;; DEPRECATED: Will be stored in the local profile in v3.0
(defvar doom-disabled-packages ()
  "A list of packages that should be ignored by `use-package!' and `after!'.")


;;
;;; Package management API

(defun doom--ensure-straight (recipe pin)
  (letenv! (("GIT_CONFIG" nil)
            ("GIT_CONFIG_NOSYSTEM" "1")
            ("GIT_CONFIG_GLOBAL" (or (getenv "DOOMGITCONFIG")
                                     "/dev/null")))
    (let ((repo-dir (doom-path straight-base-dir "straight/repos/straight.el"))
          (repo-url (concat "http" (if gnutls-verify-error "s")
                            "://github.com/"
                            (or (plist-get recipe :repo) "radian-software/straight.el")))
          (branch (or (plist-get recipe :branch) straight-repository-branch))
          (call (if init-file-debug
                    (lambda (&rest args)
                      (print! "%s" (cdr (apply #'doom-call-process args))))
                  (lambda (&rest args)
                    (apply #'doom-call-process args)))))
      (unless (file-directory-p repo-dir)
        (save-match-data
          (unless (executable-find "git")
            (user-error "Git isn't present on your system. Cannot proceed."))
          (let* ((version (cdr (doom-call-process "git" "version")))
                 (version
                  (and (string-match "\\_<[0-9]+\\.[0-9]+\\(\\.[0-9]+\\)\\_>" version)
                       (match-string 0 version))))
            (if version
                (when (version< version "2.23")
                  (user-error "Git %s detected! Doom requires git 2.23 or newer!"
                              version)))))
        (print! (start "Installing straight..."))
        (print-group!
         (cl-destructuring-bind (depth . options)
             (ensure-list straight-vc-git-default-clone-depth)
           (let ((branch-switch (if (memq 'single-branch options)
                                    "--single-branch"
                                  "--no-single-branch")))
             (cond
              ((eq 'full depth)
               (funcall call "git" "clone" "--origin" "origin"
                        branch-switch repo-url repo-dir))
              ((integerp depth)
               (if (null pin)
                   (progn
                     (when (file-directory-p repo-dir)
                       (delete-directory repo-dir 'recursive))
                     (funcall call "git" "clone" "--origin" "origin" repo-url
                              "--no-checkout" repo-dir
                              "--depth" (number-to-string depth)
                              branch-switch
                              "--no-tags"
                              "--branch" straight-repository-branch))
                 (make-directory repo-dir 'recursive)
                 (let ((default-directory repo-dir))
                   (funcall call "git" "init")
                   (funcall call "git" "branch" "-m" straight-repository-branch)
                   (funcall call "git" "remote" "add" "origin" repo-url
                            "--master" straight-repository-branch)
                   (funcall call "git" "fetch" "origin" pin
                            "--depth" (number-to-string depth)
                            "--no-tags")
                   (funcall call "git" "reset" "--hard" pin)))))))))
      (require 'straight (concat repo-dir "/straight.el"))
      (doom-log "Initializing recipes")
      (mapc #'straight-use-recipes
            '((org-elpa :local-repo nil)
              (melpa              :type git :host github
                                  :repo "melpa/melpa"
                                  :build nil)
              (nongnu-elpa        :type git
                                  :repo "https://git.savannah.gnu.org/git/emacs/nongnu.git"
                                  :local-repo "nongnu-elpa"
                                  :build nil)
              (gnu-elpa-mirror    :type git :host github
                                  :repo "emacs-straight/gnu-elpa-mirror"
                                  :build nil)
              (el-get             :type git :host github
                                  :repo "dimitri/el-get"
                                  :build nil)
              (emacsmirror-mirror :type git :host github
                                  :repo "emacs-straight/emacsmirror-mirror"
                                  :build nil))))))

(defun doom--ensure-core-packages (packages)
  (doom-log "Installing core packages")
  (dolist (package packages)
    (let* ((name (car package))
           (repo (symbol-name name)))
      (when-let (recipe (plist-get (cdr package) :recipe))
        (straight-override-recipe (cons name recipe))
        (when-let (local-repo (plist-get recipe :local-repo))
          (setq repo local-repo)))
      (print-group!
       ;; Only clone the package, don't build them. Straight hasn't been fully
       ;; configured by this point.
       (straight-use-package name nil t))
      ;; In case the package hasn't been built yet.
      (or (member (directory-file-name (straight--build-dir (symbol-name name)))
                  load-path)
          (add-to-list 'load-path (directory-file-name (straight--repos-dir repo)))))))

;;;###autoload
(defun doom-initialize-core-packages (&optional force-p)
  "Ensure `straight' is installed and was compiled with this version of Emacs."
  (when (or force-p (null (bound-and-true-p straight-recipe-repositories)))
    (doom-log "Initializing straight")
    (let ((packages (doom-package-list '((:doom)))))
      (cl-destructuring-bind (&key recipe pin &allow-other-keys)
          (alist-get 'straight packages)
        (doom--ensure-straight recipe pin))
      (doom--ensure-core-packages
       (seq-filter (fn! (eq (plist-get (cdr %) :type) 'core))
                   packages)))))

;;;###autoload
(defun doom-initialize-packages (&optional force-p)
  "Process all packages, essential and otherwise, if they haven't already been.

If FORCE-P is non-nil, do it anyway.

This ensures `doom-packages' is populated and `straight' recipes are properly
processed."
  (doom-initialize-core-packages force-p)
  (when (or force-p (not (bound-and-true-p package--initialized)))
    (doom-log "Initializing package.el")
    (require 'package)
    (package-initialize)
    (unless package--initialized
      (error "Failed to initialize package.el")))
  (when (or force-p (null doom-packages))
    (doom-log "Initializing straight.el")
    (setq doom-disabled-packages nil
          doom-packages (doom-package-list))
    (let (packages)
      (dolist (package doom-packages)
        (cl-destructuring-bind
            (name &key recipe disable ignore &allow-other-keys) package
          (if ignore
              (straight-override-recipe (cons name '(:type built-in)))
            (if disable
                (cl-pushnew name doom-disabled-packages)
              (when recipe
                (straight-override-recipe (cons name recipe)))
              (appendq! packages (cons name (straight--get-dependencies name)))))))
      (dolist (package (cl-delete-duplicates packages :test #'equal))
        (straight-register-package package)
        (let ((name (symbol-name package)))
          (add-to-list 'load-path (directory-file-name (straight--build-dir name)))
          (straight--load-package-autoloads name))))))

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
(defun doom-package-set (package prop value)
  "Set PROPERTY in PACKAGE's recipe to VALUE."
  (setf (alist-get package doom-packages)
        (plist-put (alist-get package doom-packages)
                   prop value)))

;;;###autoload
(defun doom-package-recipe (package &optional prop nil-value)
  "Returns the `straight' recipe PACKAGE was registered with."
  (let* ((recipe (straight-recipes-retrieve package))
         (plist (doom-plist-merge
                 (plist-get (alist-get package doom-packages) :recipe)
                 (cdr (if (memq (car recipe) '(quote \`))
                          (eval recipe t)
                        recipe)))))
    (if prop
        (if (plist-member plist prop)
            (plist-get plist prop)
          nil-value)
      plist)))

;;;###autoload
(defun doom-package-recipe-repo (package)
  "Resolve and return PACKAGE's (symbol) local-repo property."
  (if-let* ((recipe (copy-sequence (doom-package-recipe package)))
            (recipe (if (and (not (plist-member recipe :type))
                             (memq (plist-get recipe :host) '(github gitlab bitbucket)))
                        (plist-put recipe :type 'git)
                      recipe))
            (repo (if-let (local-repo (plist-get recipe :local-repo))
                      (directory-file-name local-repo)
                    (ignore-errors (straight-vc-local-repo-name recipe)))))
      repo
    (symbol-name package)))

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
(defun doom-package-dependencies (package &optional recursive noerror)
  "Return a list of dependencies for a package.

If RECURSIVE is `tree', return a tree of dependencies.
If RECURSIVE is nil, only return PACKAGE's immediate dependencies.
If NOERROR, return nil in case of error."
  (cl-check-type package symbol)
  (let ((deps (straight-dependencies (symbol-name package))))
    (pcase recursive
      (`tree deps)
      (`t (flatten-list deps))
      (`nil (cl-remove-if #'listp deps)))))

;;;###autoload
(defun doom-package-depending-on (package &optional noerror)
  "Return a list of packages that depend on PACKAGE.

If PACKAGE (a symbol) isn't installed, throw an error, unless NOERROR is
non-nil."
  (cl-check-type package symbol)
  ;; can't get dependencies for built-in packages
  (unless (or (doom-package-build-recipe package)
              noerror)
    (error "Couldn't find %s, is it installed?" package))
  (straight-dependents (symbol-name package)))

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
(defun doom-package-is-type-p (package type)
  "TODO"
  (memq type (ensure-list (doom-package-get package :type))))

;;;###autoload
(defun doom-package-in-module-p (package category &optional module)
  "Return non-nil if PACKAGE was installed by the user's private config."
  (when-let (modules (doom-package-get package :modules))
    (or (and (not module) (assq :user modules))
        (member (cons category module) modules))))

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
        ((locate-library (symbol-name package))
         'other)))


;;; Package getters
(defun doom-packages--read (file &optional noeval noerror)
  (condition-case-unless-debug e
      (with-temp-buffer ; prevent buffer-local state from propagating
        (if (not noeval)
            (load file noerror 'nomessage 'nosuffix)
          (when (file-exists-p file)
            (insert-file-contents file)
            (with-syntax-table emacs-lisp-mode-syntax-table
              ;; Scrape `package!' blocks from FILE for a comprehensive listing of
              ;; packages used by this module.
              (while (search-forward "(package!" nil t)
                (let ((ppss (save-excursion (syntax-ppss))))
                  ;; Don't collect packages in comments or strings
                  (unless (or (nth 3 ppss)
                              (nth 4 ppss))
                    (goto-char (match-beginning 0))
                    (cl-destructuring-bind (_ name . plist)
                        (read (current-buffer))
                      (push (cons
                             name (plist-put
                                   plist :modules
                                   (list (doom-module-context-key doom-module-context))))
                            doom-packages)))))))))
    (user-error
     (user-error (error-message-string e)))
    (error
     (signal 'doom-package-error
             (list (doom-module-context-key doom-module-context)
                   file e)))))

;;;###autoload
(defun doom-package-list (&optional module-list)
  "Retrieve a list of explicitly declared packages from MODULE-LIST.

If MODULE-LIST is omitted, read enabled module list in configdepth order (see
`doom-module-set'). Otherwise, MODULE-LIST may be any symbol (or t) to mean read
all modules in `doom-modules-dir', including :doom and :user. MODULE-LIST may
also be a list of module keys."
  (let ((module-list (cond ((null module-list) (doom-module-list))
                           ((symbolp module-list) (doom-module-list 'all))
                           (module-list)))
        (packages-file doom-module-packages-file)
        doom-disabled-packages
        doom-packages)
    (letf! (defun read-packages (key)
             (with-doom-module key
               (when-let (file (doom-module-locate-path
                                key doom-module-packages-file))
                 (doom-packages--read file nil 'noerror))))
      (with-doom-context 'package
        (let ((user? (assq :user module-list)))
          (when user?
            ;; We load the private packages file twice to populate
            ;; `doom-disabled-packages' disabled packages are seen ASAP...
            (let (doom-packages)
              (read-packages (cons :user nil))))
          (mapc #'read-packages module-list)
          ;; ...Then again to ensure privately overriden packages are properly
          ;; overwritten.
          (if user? (read-packages (cons :user nil)))
          (nreverse doom-packages))))))

;;;###autoload
(defun doom-package-pinned-alist ()
  "Return an alist mapping package names (strings) to pinned commits (strings)."
  (let (alist)
    (dolist (package doom-packages alist)
      (cl-destructuring-bind (name &key disable ignore pin unpin &allow-other-keys)
          package
        (when (and (not ignore)
                   (not disable)
                   (or pin unpin))
          (setf (alist-get (file-name-nondirectory (doom-package-recipe-repo name))
                           alist nil 'remove #'equal)
                (unless unpin pin)))))))

;;;###autoload
(defun doom-package-recipe-alist ()
  "Return straight recipes for non-builtin packages with a local-repo."
  (let (recipes)
    (dolist (recipe (hash-table-values straight--recipe-cache))
      (cl-destructuring-bind (&key local-repo type &allow-other-keys)
          recipe
        (unless (or (null local-repo)
                    (eq type 'built-in))
          (push recipe recipes))))
    (nreverse recipes)))

;;;###autoload
(defun doom-package-homepage (package)
  "return the url to package's homepage (usually a repo)."
  (doom-initialize-packages)
  (or (get package 'homepage)
      (put package 'homepage
           (cond ((when-let (location (locate-library (symbol-name package)))
                    (with-temp-buffer
                      (if (string-match-p "\\.gz$" location)
                          (jka-compr-insert-file-contents location)
                        (insert-file-contents (concat (file-name-sans-extension location) ".el")
                                              nil 0 4096))
                      (let ((case-fold-search t))
                        (when (re-search-forward " \\(?:url\\|homepage\\|website\\): \\(http[^\n]+\\)\n" nil t)
                          (match-string-no-properties 1))))))
                 ((when-let ((recipe (straight-recipes-retrieve package)))
                    (straight--with-plist (straight--convert-recipe recipe)
                        (host repo)
                      (pcase host
                        (`github (format "https://github.com/%s" repo))
                        (`gitlab (format "https://gitlab.com/%s" repo))
                        (`bitbucket (format "https://bitbucket.com/%s" (plist-get plist :repo)))
                        (`git repo)
                        (_ nil)))))
                 ((or package-archive-contents
                      (progn (package-refresh-contents)
                             package-archive-contents))
                  (pcase (ignore-errors (package-desc-archive (cadr (assq package package-archive-contents))))
                    (`nil nil)
                    ("org" "https://orgmode.org")
                    ((or "melpa" "melpa-mirror")
                     (format "https://melpa.org/#/%s" package))
                    ("gnu"
                     (format "https://elpa.gnu.org/packages/%s.html" package))
                    (archive
                     (if-let (src (cdr (assoc package package-archives)))
                         (format "%s" src)
                       (user-error "%s isn't installed through any known source (%s)"
                                   package archive)))))
                 ((user-error "Can't get homepage for %S package" package))))))


;;
;;; Commands

;;;###autoload
(defun doom/reload-packages ()
  "Reload `doom-packages', `package' and `quelpa'."
  (interactive)
  ;; HACK straight.el must be loaded for this to work
  (message "Reloading packages")
  (doom-initialize-packages t)
  (message "Reloading packages...DONE"))

(defun doom--package-merge-recipes (package plist)
  (require 'straight)
  (doom-plist-merge
   (plist-get plist :recipe)
   (if-let (recipe (straight-recipes-retrieve package))
       (cdr (if (memq (car recipe) '(quote \`))
                (eval recipe t)
              recipe))
     (let ((recipe (plist-get (cdr (assq package doom-packages))
                              :recipe)))
       (if (keywordp (car recipe))
           recipe
         (cdr recipe))))))

(defun doom--package-to-bump-string (package plist)
  "Return a PACKAGE and its PLIST in 'username/repo@commit' format."
  (format "%s@%s"
          (plist-get (doom--package-merge-recipes package plist) :repo)
          (substring-no-properties (plist-get plist :pin) 0 12)))

(defun doom--package-at-point (&optional point)
  "Return the package and plist from the (package! PACKAGE PLIST...) at point."
  (save-match-data
    (save-excursion
      (and point (goto-char point))
      (while (and (or (atom (sexp-at-point))
                      (doom-point-in-string-or-comment-p))
                  (search-backward "(" nil t)))
      (when (eq (car-safe (sexp-at-point)) 'package!)
        (cl-destructuring-bind (beg . end)
            (bounds-of-thing-at-point 'sexp)
          (let* ((doom-packages nil)
                 (buffer-file-name
                  (or buffer-file-name
                      (bound-and-true-p org-src-source-file-name)))
                 (package
                  (with-doom-context 'package
                    (with-doom-module (doom-module-from-path buffer-file-name)
                      (eval (sexp-at-point) t)))))
            (list :beg beg
                  :end end
                  :package (car package)
                  :plist (cdr package))))))))

;;;###autoload
(defun doom/bumpify-package-at-point ()
  "Convert `package!' call at point to a bump string."
  (interactive)
  (cl-destructuring-bind (&key package plist beg end)
      (doom--package-at-point)
    (when-let (str (doom--package-to-bump-string package plist))
      (goto-char beg)
      (delete-region beg end)
      (insert str))))

;;;###autoload
(defun doom/bumpify-packages-in-buffer ()
  "Convert all `package!' calls in buffer into bump strings."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "(package!" nil t)
      (unless (doom-point-in-string-or-comment-p)
        (doom/bumpify-package-at-point)))))

;;;###autoload
(defun doom/bump-package-at-point (&optional select)
  "Inserts or updates a `:pin' for the `package!' statement at point.
Grabs the latest commit id of the package using 'git'."
  (interactive "P")
  (doom-initialize-packages)
  (cl-destructuring-bind (&key package plist beg end)
      (or (doom--package-at-point)
          (user-error "Not on a `package!' call"))
    (let* ((recipe (doom--package-merge-recipes package plist))
           (branch (plist-get recipe :branch))
           (oldid (or (plist-get plist :pin)
                      (doom-package-get package :pin)))
           (url (straight-vc-git--destructure recipe (upstream-repo upstream-host)
                  (straight-vc-git--encode-url upstream-repo upstream-host)))
           (id (or (when url
                     (cdr (doom-call-process
                           "git" "ls-remote" url
                           (unless select branch))))
                   (user-error "Couldn't find a recipe for %s" package)))
           (id (car (split-string
                     (if select
                         (completing-read "Commit: " (split-string id "\n" t))
                       id)))))
      (when (and oldid
                 (plist-member plist :pin)
                 (equal oldid id))
        (user-error "%s: no update necessary" package))
      (save-excursion
        (if (re-search-forward ":pin +\"\\([^\"]+\\)\"" end t)
            (replace-match id t t nil 1)
          (goto-char (1- end))
          (insert " :pin " (prin1-to-string id))))
      (cond ((not oldid)
             (message "%s: → %s" package (substring id 0 10)))
            ((< (length oldid) (length id))
             (message "%s: extended to %s..." package id))
            ((message "%s: %s → %s"
                      package
                      (substring oldid 0 10)
                      (substring id 0 10)))))))

;;;###autoload
(defun doom/bump-packages-in-buffer (&optional select)
  "Inserts or updates a `:pin' to all `package!' statements in current buffer.
If SELECT (prefix arg) is non-nil, prompt you to choose a specific commit for
each package."
  (interactive "P")
  (save-excursion
    (goto-char (point-min))
    (doom-initialize-packages)
    (let (packages)
      (while (search-forward "(package! " nil t)
        (unless (let ((ppss (syntax-ppss)))
                  (or (nth 4 ppss)
                      (nth 3 ppss)
                      (save-excursion
                        (and (goto-char (match-beginning 0))
                             (not (plist-member (sexp-at-point) :pin))))))
          (condition-case e
              (push (doom/bump-package-at-point select) packages)
            (user-error (message "%s" (error-message-string e))))))
      (if packages
          (message "Updated %d packages\n- %s" (length packages) (string-join packages "\n- "))
        (message "No packages to update")))))

;;;###autoload
(defun doom/bump-module (category &optional module select)
  "Bump packages in CATEGORY MODULE.
If SELECT (prefix arg) is non-nil, prompt you to choose a specific commit for
each package."
  (interactive
   (let* ((module (completing-read
                   "Bump module: "
                   (let ((modules (doom-module-list 'all)))
                     (mapcar (lambda (m)
                               (if (listp m)
                                   (format "%s %s" (car m) (cdr m))
                                 (format "%s" m)))
                             (append (delete-dups (mapcar #'car modules))
                                     modules)))
                   nil t nil nil))
          (module (split-string module " " t)))
     (list (intern (car module))
           (ignore-errors (intern (cadr module)))
           current-prefix-arg)))
  (mapc (lambda! (key)
          (if-let (packages-file (doom-module-locate-path key doom-module-packages-file))
              (with-current-buffer
                  (or (get-file-buffer packages-file)
                      (find-file-noselect packages-file))
                (doom/bump-packages-in-buffer select)
                (save-buffer))
            (message "Module %s has no packages.el file" key)))
        (if module
            (list (cons category module))
          (cl-remove-if-not (lambda (m) (eq (car m) category))
                            (doom-module-list 'all)))))

;;;###autoload
(defun doom/bump-package (package)
  "Bump PACKAGE in all modules that install it."
  (interactive
   (list (intern (completing-read "Bump package: "
                          (mapcar #'car (doom-package-list 'all))))))
  (let* ((packages (doom-package-list 'all))
         (modules (plist-get (alist-get package packages) :modules)))
    (unless modules
      (user-error "This package isn't installed by any Doom module"))
    (dolist (module modules)
      (when (doom-module-locate-path module doom-module-packages-file)
        (doom/bump-module (car module) (cdr module))))))

;;;###autoload
(defun doom/bumpify-diff (&optional interactive)
  "Copy user/repo@hash -> user/repo@hash's of changed packages to clipboard.

Must be run from a magit diff buffer."
  (interactive (list 'interactive))
  (save-window-excursion
    (magit-diff-staged)
    (unless (eq major-mode 'magit-diff-mode)
      (user-error "Not in a magit diff buffer"))
    (goto-char (point-min))
    (letf! (defun read-package ()
             (let* ((file (magit-file-at-point))
                    (visited? (if file (get-file-buffer file))))
               (save-window-excursion
                 (call-interactively #'magit-diff-visit-file)
                 (unwind-protect
                     (and (or (looking-at-p "(package!")
                              (re-search-forward "(package! " (line-end-position) t)
                              (re-search-backward "(package! " nil t))
                          (let* ((buffer-file-name file)
                                 (plist (doom--package-at-point)))
                            (cons (plist-get plist :package)
                                  plist)))
                   (unless visited?
                     (kill-current-buffer))))))
      (let (targets
            before
            after
            lines
            errors)
        (save-excursion
          (while (re-search-forward "^modified +\\(.+\\)$" nil t)
            (cl-pushnew (doom-module-from-path (match-string 1)) targets
                        :test #'equal)))
        (save-excursion
          (while (re-search-forward "^-" nil t)
            (when-let (pkg (read-package))
              (cl-pushnew pkg before :test #'equal))))
        (save-excursion
          (while (re-search-forward "^+" nil t)
            (when-let (pkg (read-package))
              (cl-pushnew pkg after :test #'equal))))
        (unless (= (length before) (length after))
          (user-error "Uneven number of packages being bumped"))
        (dolist (p1 before)
          (when (and (listp p1) (plist-get (cdr p1) :package))
            (cl-destructuring-bind (package &key plist _beg _end &allow-other-keys) p1
              (let ((p2 (cdr (assq package after))))
                (if (null p2)
                    (push package errors)
                  (let ((bstr1 (doom--package-to-bump-string package plist))
                        (bstr2 (doom--package-to-bump-string package (plist-get p2 :plist))))
                    (cl-pushnew (format "%s -> %s" bstr1 bstr2) lines :test #'equal)))))))
        (if (null lines)
            (user-error "No bumps to bumpify")
          (prog1 (funcall (if interactive #'kill-new #'identity)
                          (format "bump: %s\n\n%s"
                                  (mapconcat (lambda (x)
                                               (mapconcat #'symbol-name x " "))
                                             (cl-loop with alist = ()
                                                      for (category . module) in (reverse targets)
                                                      do (setf (alist-get category alist)
                                                               (append (alist-get category alist) (list module)))
                                                      finally return alist)
                                             " ")
                                  (string-join (sort (reverse lines) #'string-lessp)
                                               "\n")))
            (when interactive
              (message "Copied to clipboard"))))))))

;;;###autoload
(defun doom/commit-bumps ()
  "Create a pre-filled magit commit for currently bumped packages."
  (interactive)
  (magit-commit-create
   (list "-e" "-m" (doom/bumpify-diff))))

(provide 'doom-lib '(packages))
;;; packages.el ends here
