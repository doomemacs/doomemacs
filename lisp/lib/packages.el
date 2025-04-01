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

(require 'comp nil t)
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
  (with-environment-variables
      (("GIT_CONFIG" nil)
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
              (nongnu-elpa        :type git :host github
                                  :repo "emacsmirror/nongnu_elpa"
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
              (cl-callf append packages (cons name (straight--get-dependencies name)))))))
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
            (repo (if-let* ((local-repo (plist-get recipe :local-repo)))
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
                     (if-let* ((src (cdr (assoc package package-archives))))
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
   (if-let* ((recipe (straight-recipes-retrieve package)))
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
          (if-let* ((packages-file (doom-module-locate-path key doom-module-packages-file)))
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


;;
;;; CLI API

(defun doom-packages--same-commit-p (abbrev-ref ref)
  (and (stringp abbrev-ref)
       (stringp ref)
       (string-match-p (concat "^" (regexp-quote abbrev-ref))
                       ref)))

(defun doom-packages--abbrev-commit (commit &optional full)
  (if full commit (substring commit 0 7)))

(defun doom-packages--commit-log-between (start-ref end-ref)
  (straight--process-with-result
   (straight--process-run
    "git" "log" "--oneline" "--no-merges"
    end-ref (concat "^" (regexp-quote start-ref)))
   (if success
       (string-trim-right (or stdout ""))
     (format "ERROR: Couldn't collect commit list because: %s" stderr))))

(defmacro doom-packages--straight-with (form &rest body)
  (declare (indent 1))
  `(let-alist
       (let* ((buffer (straight--process-buffer))
              (start  (with-current-buffer buffer (point-max)))
              (retval ,form)
              (output (with-current-buffer buffer (buffer-substring start (point-max)))))
         (save-match-data
           (list (cons 'it      retval)
                 (cons 'stdout  (substring-no-properties output))
                 (cons 'success (if (string-match "\n+\\[Return code: \\([0-9-]+\\)\\]\n+" output)
                                    (string-to-number (match-string 1 output))))
                 (cons 'output  (string-trim output
                                             "^\\(\\$ [^\n]+\n\\)*\n+"
                                             "\n+\\[Return code: [0-9-]+\\]\n+")))))
     ,@body))

(defun doom-packages--barf-if-incomplete ()
  (let ((straight-safe-mode t))
    (condition-case _ (straight-check-all)
      (error (user-error "Package state is incomplete. Run 'doom sync' first")))))

(defmacro doom-packages--with-recipes (recipes binds &rest body)
  (declare (indent 2))
  (let ((recipe-var  (make-symbol "recipe"))
        (recipes-var (make-symbol "recipes")))
    `(let* ((,recipes-var ,recipes)
            (built ())
            (straight-use-package-pre-build-functions
             (cons (lambda (pkg &rest _) (cl-pushnew pkg built :test #'equal))
                   straight-use-package-pre-build-functions)))
       (dolist (,recipe-var ,recipes-var (nreverse built))
         (cl-block nil
           (straight--with-plist (append (list :recipe ,recipe-var) ,recipe-var)
               ,(ensure-list binds)
             ,@body))))))

(defvar doom-packages--cli-updated-recipes nil)
(defun doom-packages--cli-recipes-update ()
  "Updates straight and recipe repos."
  (unless doom-packages--cli-updated-recipes
    (straight--make-build-cache-available)
    (print! (start "Updating recipe repos..."))
    (print-group!
     (doom-packages--with-recipes
      (delq
       nil (mapcar (doom-rpartial #'gethash straight--repo-cache)
                   (mapcar #'symbol-name straight-recipe-repositories)))
      (recipe package type local-repo)
      (let ((esc (if init-file-debug "" "\033[1A"))
            (ref (straight-vc-get-commit type local-repo))
            newref output)
        (print! (start "\rUpdating recipes for %s...%s") package esc)
        (doom-packages--straight-with (straight-vc-fetch-from-remote recipe)
          (when .it
            (setq output .output)
            (straight-merge-package package)
            (unless (equal ref (setq newref (straight-vc-get-commit type local-repo)))
              (print! (success "\r%s updated (%s -> %s)")
                      package
                      (doom-packages--abbrev-commit ref)
                      (doom-packages--abbrev-commit newref))
              (unless (string-empty-p output)
                (print-group! (print! (item "%s" output))))))))))
    (setq straight--recipe-lookup-cache (make-hash-table :test #'eq)
          doom-packages--cli-updated-recipes t)))

(defvar doom-packages--eln-output-expected nil)

(defvar doom-packages--eln-output-path (car (bound-and-true-p native-comp-eln-load-path)))

(defun doom-packages--eln-file-name (file)
  "Return the short .eln file name corresponding to `file'."
  (file-name-concat
   comp-native-version-dir
   (file-name-nondirectory
    (comp-el-to-eln-filename file))))

(defun doom-packages--eln-output-file (eln-name)
  "Return the expected .eln file corresponding to `eln-name'."
  (file-name-concat doom-packages--eln-output-path eln-name))

(defun doom-packages--eln-error-file (eln-name)
  "Return the expected .error file corresponding to `eln-name'."
  (file-name-concat doom-packages--eln-output-path eln-name ".error"))

(defun doom-packages--find-eln-file (eln-name)
  "Find `eln-name' on the `native-comp-eln-load-path'."
  (cl-some (fn! (file-exists-p! eln-name %))
           native-comp-eln-load-path))

(defun doom-packages--elc-file-outdated-p (file)
  "Check whether the corresponding .elc for `file' is outdated."
  (let ((elc-file (byte-compile-dest-file file)))
    ;; NOTE Ignore missing elc files, they could be missing due to
    ;;   `no-byte-compile'. Rebuilding unnecessarily is expensive.
    (when (and (file-exists-p elc-file)
               (file-newer-than-file-p file elc-file))
      (doom-log "packages:elc: %s is newer than %s" file elc-file)
      t)))

(defun doom-packages--eln-file-outdated-p (file)
  "Check whether the corresponding .eln for `file' is outdated."
  (when (file-exists-p file)
    (let* ((eln-name (doom-packages--eln-file-name file))
           (eln-file (doom-packages--find-eln-file eln-name))
           (error-file (doom-packages--eln-error-file eln-name)))
      (cond (eln-file
             (when (file-newer-than-file-p file eln-file)
               (doom-log "packages:eln: %s is newer than %s" file eln-file)
               t))
            ((file-exists-p error-file)
             (when (file-newer-than-file-p file error-file)
               (doom-log "packages:eln: %s is newer than %s" file error-file)
               t))))))

(defun doom-packages--native-compile-done-h (file)
  "Callback fired when an item has finished async compilation."
  (when file
    (let* ((eln-name (doom-packages--eln-file-name file))
           (eln-file (doom-packages--eln-output-file eln-name))
           (error-file (doom-packages--eln-error-file eln-name)))
      (if (file-exists-p eln-file)
          (doom-log "packages:nativecomp: Compiled %s" eln-file)
        (let ((error-dir (file-name-directory error-file)))
          (if (not (file-writable-p error-dir))
              (doom-log "packages:nativecomp: failed to write %s" error-file)
            (make-directory error-dir 'parents)
            (write-region "" nil error-file)
            (doom-log "packages:nativecomp: wrote %s" error-file)))))))

(defun doom-packages--wait-for-native-compile-jobs ()
  "Wait for all pending async native compilation jobs."
  (cl-loop with previous = 0
           with timeout = 30
           with timer = 0
           for pending = (+ (length comp-files-queue)
                            (if (functionp 'comp--async-runnings)
                                (comp--async-runnings)
                              (comp-async-runnings)))
           while (not (zerop pending))
           if (/= previous pending) do
           (print! (start "\rNatively compiling %d files...\033[1A" pending))
           (setq previous pending
                 timer 0)
           else do
           (let ((inhibit-message t))
             (if (> timer timeout)
                 (cl-loop for file-name being each hash-key of comp-async-compilations
                          for prc = (gethash file-name comp-async-compilations)
                          unless (process-live-p prc)
                          do (setq timer 0)
                          and do (print! (warn "Native compilation of %S timed out" (path file-name)))
                          and return (kill-process prc))
               (cl-incf timer 0.1))
             (sleep-for 0.1))))

(defun doom-packages--write-missing-eln-errors ()
  "Write .error files for any expected .eln files that are missing."
  (cl-loop for file in doom-packages--eln-output-expected
           for eln-name = (doom-packages--eln-file-name file)
           for eln-file = (doom-packages--eln-output-file eln-name)
           for error-file = (doom-packages--eln-error-file eln-name)
           for error-dir = (file-name-directory error-file)
           unless (or (file-exists-p eln-file)
                      (file-newer-than-file-p error-file file)
                      (not (file-writable-p error-dir)))
           do (make-directory error-dir 'parents)
           (write-region "" nil error-file)
           (doom-log "Wrote %s" error-file))
  (setq doom-packages--eln-output-expected nil))

(defun doom-packages--compile-site-files ()
  "Queue async compilation for all non-doom Elisp files."
  (cl-loop with paths = (cl-loop for path in load-path
                                 unless (file-in-directory-p path doom-local-dir)
                                 collect path)
           for file in (doom-files-in paths :match "\\.el\\(?:\\.gz\\)?$")
           if (and (file-exists-p (byte-compile-dest-file file))
                   (not (doom-packages--find-eln-file (doom-packages--eln-file-name file)))
                   (not (cl-some (fn! (string-match-p % file))
                                 native-comp-deferred-compilation-deny-list))) do
           (doom-log "Compiling %s" file)
           (native-compile-async file)))

(defun doom-packages-ensure (&optional force-p)
  "Ensure packages are installed, built"
  (doom-initialize-packages)
  (if (not (file-directory-p (straight--repos-dir)))
      (print! (start "Installing all packages for the first time (this may take a while)..."))
    (if force-p
        (print! (start "Rebuilding all packages (this may take a while)..."))
      (print! (start "Ensuring packages are installed and built..."))))
  (print-group!
    (let ((straight-check-for-modifications
           (when (file-directory-p (straight--modified-dir))
             '(find-when-checking)))
          (straight--allow-find
           (and straight-check-for-modifications
                (executable-find straight-find-executable)
                t))
          (straight--packages-not-to-rebuild
           (or straight--packages-not-to-rebuild (make-hash-table :test #'equal)))
          (straight--packages-to-rebuild
           (or (if force-p :all straight--packages-to-rebuild)
               (make-hash-table :test #'equal)))
          (recipes (doom-package-recipe-alist))
          (pinned (doom-package-pinned-alist)))
      (add-hook 'native-comp-async-cu-done-functions #'doom-packages--native-compile-done-h)
      (straight--make-build-cache-available)
      (if-let* ((built
                 (doom-packages--with-recipes recipes (package local-repo recipe)
                   (let ((repo-dir (straight--repos-dir (or local-repo package)))
                         (build-dir (straight--build-dir package)))
                     (unless force-p
                       ;; Ensure packages w/ outdated files/bytecode are rebuilt
                       (let* ((build (if (plist-member recipe :build)
                                         (plist-get recipe :build)
                                       t))
                              (want-byte-compile
                               (or (eq build t)
                                   (memq 'compile build)))
                              (want-native-compile
                               (or (eq build t)
                                   (memq 'native-compile build))))
                         (and (eq (car-safe build) :not)
                              (setq want-byte-compile (not want-byte-compile)
                                    want-native-compile (not want-native-compile)))
                         (when (or (not (featurep 'native-compile))
                                   (not straight--native-comp-available))
                           (setq want-native-compile nil))
                         (and (or want-byte-compile want-native-compile)
                              (or (file-newer-than-file-p repo-dir build-dir)
                                  (file-exists-p (straight--modified-dir package))
                                  (cl-loop with outdated = nil
                                           for file in (doom-files-in build-dir :match "\\.el$" :full t)
                                           if (or (if want-byte-compile   (doom-packages--elc-file-outdated-p file))
                                                  (if want-native-compile (doom-packages--eln-file-outdated-p file)))
                                           do (setq outdated t)
                                           (when want-native-compile
                                             (push file doom-packages--eln-output-expected))
                                           finally return outdated))
                              (puthash package t straight--packages-to-rebuild))))
                     (unless (file-directory-p repo-dir)
                       (doom-packages--cli-recipes-update))
                     (condition-case-unless-debug e
                         (let ((straight-vc-git-post-clone-hook
                                (cons (lambda! (&key commit)
                                        (print-group!
                                          (if-let* ((pin (cdr (assoc package pinned))))
                                              (print! (item "Pinned to %s") pin)
                                            (when commit
                                              (print! (item "Checked out %s") commit)))))
                                      straight-vc-git-post-clone-hook)))
                           (straight-use-package (intern package))
                           (when (file-in-directory-p repo-dir straight-base-dir)
                             ;; HACK: Straight can sometimes fail to clone a
                             ;;   repo, leaving behind an empty directory which,
                             ;;   in future invocations, it will assume
                             ;;   indicates a successful clone (causing load
                             ;;   errors later).
                             (let ((try 0))
                               (while (not (file-directory-p (doom-path repo-dir ".git")))
                                 (when (= try 3)
                                   (error "Failed to clone package"))
                                 (print! (warn "Failed to clone %S, trying again (attempt #%d)...") package (1+ try))
                                 (delete-directory repo-dir t)
                                 (delete-directory build-dir t)
                                 (straight-use-package (intern package))
                                 (cl-incf try)))
                             ;; HACK: Line encoding issues can plague repos with
                             ;;   dirty worktree prompts when updating packages
                             ;;   or "Local variables entry is missing the
                             ;;   suffix" errors when installing them (see
                             ;;   #2637), so have git handle conversion by
                             ;;   force.
                             (when doom--system-windows-p
                               (let ((default-directory repo-dir))
                                 (straight--process-run "git" "config" "core.autocrlf" "true")))))
                       (error
                        (signal 'doom-package-error (list package e))))))))
          (progn
            (when (and (featurep 'native-compile)
                       straight--native-comp-available)
              (doom-packages--compile-site-files)
              (doom-packages--wait-for-native-compile-jobs)
              (doom-packages--write-missing-eln-errors))
            ;; HACK: Every time you save a file in a package that straight
            ;;   tracks, it is recorded in ~/.emacs.d/.local/straight/modified/.
            ;;   Typically, straight will clean these up after rebuilding, but
            ;;   Doom's use-case circumnavigates that, leaving these files there
            ;;   and causing a rebuild of those packages each time `doom sync'
            ;;   or similar is run, so we clean it up ourselves:
            (delete-directory (straight--modified-dir) 'recursive)
            (print! (success "\rBuilt %d package(s)") (length built)))
        (print! (item "No packages need attention"))
        nil))))

(defun doom-packages-update (&optional pinned-only-p)
  "Updates packages."
  (doom-initialize-packages)
  (doom-packages--barf-if-incomplete)
  (let* ((repo-dir (straight--repos-dir))
         (pinned (doom-package-pinned-alist))
         (recipes (doom-package-recipe-alist))
         (packages-to-rebuild (make-hash-table :test 'equal))
         (repos-to-rebuild (make-hash-table :test 'equal))
         (total (length recipes))
         (esc (if init-file-debug "" "\033[1A"))
         (i 0))
    (if pinned-only-p
        (print! (start "Updating pinned packages..."))
      (print! (start "Updating all packages (this may take a while)...")))
    (doom-packages--with-recipes recipes (recipe package type local-repo)
      (cl-incf i)
      (print-group!
       (unless (straight--repository-is-available-p recipe)
         (print! (error "(%d/%d) Couldn't find local repo for %s") i total package)
         (cl-return))
       (when (gethash local-repo repos-to-rebuild)
         (puthash package t packages-to-rebuild)
         (print! (success "(%d/%d) %s was updated indirectly (with %s)") i total package local-repo)
         (cl-return))
       (let ((default-directory (straight--repos-dir local-repo)))
         (unless (file-in-directory-p default-directory repo-dir)
           (print! (warn "(%d/%d) Skipping %s because it is out-of-tree...") i total package)
           (cl-return))
         (when (eq type 'git)
           (unless (file-exists-p ".git")
             (error "%S is not a valid repository" package)))
         (when (and pinned-only-p (not (assoc local-repo pinned)))
           (cl-return))
         (condition-case-unless-debug e
             (let ((ref (straight-vc-get-commit type local-repo))
                   (target-ref
                    (cdr (or (assoc local-repo pinned)
                             (assoc package pinned))))
                   commits
                   output)
               (or (cond
                    ((not (stringp target-ref))
                     (print! (start "\r(%d/%d) Fetching %s...%s") i total package esc)
                     (doom-packages--straight-with (straight-vc-fetch-from-remote recipe)
                       (when .it
                         (straight-merge-package package)
                         (setq target-ref (straight-vc-get-commit type local-repo))
                         (setq output (doom-packages--commit-log-between ref target-ref)
                               commits (length (split-string output "\n" t)))
                         (or (not (doom-packages--same-commit-p target-ref ref))
                             (cl-return)))))

                    ((doom-packages--same-commit-p target-ref ref)
                     (print! (item "\r(%d/%d) %s is up-to-date...%s") i total package esc)
                     (cl-return))

                    ((if (straight-vc-commit-present-p recipe target-ref)
                         (print! (start "\r(%d/%d) Checking out %s (%s)...%s")
                                 i total package (doom-packages--abbrev-commit target-ref) esc)
                       (print! (start "\r(%d/%d) Fetching %s...%s") i total package esc)
                       (and (straight-vc-fetch-from-remote recipe)
                            (straight-vc-commit-present-p recipe target-ref)))
                     (straight-vc-check-out-commit recipe target-ref)
                     (or (not (eq type 'git))
                         (setq output (doom-packages--commit-log-between ref target-ref)
                               commits (length (split-string output "\n" t))))
                     (doom-packages--same-commit-p target-ref (straight-vc-get-commit type local-repo)))

                    ((print! (start "\r(%d/%d) Re-cloning %s...") i total local-repo esc)
                     (let ((repo (straight--repos-dir local-repo))
                           (straight-vc-git-default-clone-depth 'full))
                       (delete-directory repo 'recursive)
                       (print-group!
                         (straight-use-package (intern package) nil 'no-build))
                       (prog1 (file-directory-p repo)
                         (or (not (eq type 'git))
                             (setq output (doom-packages--commit-log-between ref target-ref)
                                   commits (length (split-string output "\n" t))))))))
                   (progn
                     (print! (warn "\r(%d/%d) Failed to fetch %s")
                             i total local-repo)
                     (unless (string-empty-p output)
                       (print-group! (print! (item "%s" output))))
                     (cl-return)))
               (puthash local-repo t repos-to-rebuild)
               ;; HACK: Rebuild all packages that depend on PACKAGE after
               ;;   updating it. This ensures their bytecode don't contain stale
               ;;   references to symbols in silent dependencies.
               ;; TODO: Allow `package!' to control this.
               ;; TODO: Add cache+optimization step for this rebuild table.
               (letf! ((dependents (straight-dependents package))
                       (n 0)
                       (defun* add-to-rebuild (tree)
                         (cond ((null tree) nil)
                               ((stringp tree)
                                (unless (gethash tree packages-to-rebuild)
                                  (cl-incf n 1)
                                  (puthash tree t packages-to-rebuild)))
                               ((listp tree)
                                (add-to-rebuild (car tree))
                                (add-to-rebuild (cdr tree))))))
                 (add-to-rebuild dependents)
                 (puthash package t packages-to-rebuild)
                 (print! (success "\r(%d/%d) %s: %s -> %s%s%s")
                         i total local-repo
                         (doom-packages--abbrev-commit ref)
                         (doom-packages--abbrev-commit target-ref)
                         (if (and (integerp commits) (> commits 0))
                             (format " [%d commit(s)]" commits)
                           "")
                         (if (> n 0)
                             (format " (w/ %d dependents)" n)
                           "")))
               (unless (string-empty-p output)
                 (let ((lines (split-string output "\n")))
                   (setq output
                         (if (> (length lines) 20)
                             (concat (string-join (cl-subseq (butlast lines 1) 0 20) "\n")
                                     "\n[...]")
                           output)))
                 (print-group! (print! "%s" (indent output 2)))))
           (user-error
            (signal 'user-error (error-message-string e)))
           (error
            (signal 'doom-package-error (list package e)))))))
    (print-group!
     (if (hash-table-empty-p packages-to-rebuild)
         (ignore (print! (success "\rAll %d packages are up-to-date") total))
       (doom-packages--cli-recipes-update)
       (straight--transaction-finalize)
       (let ((default-directory (straight--build-dir)))
         (mapc (doom-rpartial #'delete-directory 'recursive)
               (hash-table-keys packages-to-rebuild)))
       (print! (success "\rUpdated %d package(s)")
               (hash-table-count packages-to-rebuild))
       (doom-packages-ensure)
       t))))


;;; PURGE (for the emperor)
(defun doom-packages--purge-build (build)
  (let ((build-dir (straight--build-dir build)))
    (delete-directory build-dir 'recursive)
    (if (file-directory-p build-dir)
        (ignore (print! (error "Failed to purg build/%s" build)))
      (print! (success "Purged build/%s" build))
      t)))

(defun doom-packages--purge-builds (builds)
  (if (not builds)
      (prog1 0
        (print! (item "No builds to purge")))
    (print! (start "Purging straight builds..." (length builds)))
    (print-group!
     (length
      (delq nil (mapcar #'doom-packages--purge-build builds))))))

(cl-defun doom-packages--regraft-repo (repo)
  (unless repo
    (error "No repo specified for regrafting"))
  (let ((default-directory (straight--repos-dir repo)))
    (unless (file-directory-p ".git")
      (print! (warn "\rrepos/%s is not a git repo, skipping" repo))
      (cl-return))
    (unless (file-in-directory-p default-directory straight-base-dir)
      (print! (warn "\rSkipping repos/%s because it is local" repo))
      (cl-return))
    (let ((before-size (doom-directory-size default-directory)))
      (doom-call-process "git" "reset" "--hard")
      (doom-call-process "git" "clean" "-ffd")
      (if (not (zerop (car (doom-call-process "git" "replace" "--graft" "HEAD"))))
          (print! (item "\rrepos/%s is already compact\033[1A" repo))
        (doom-call-process "git" "reflog" "expire" "--expire=all" "--all")
        (doom-call-process "git" "gc" "--prune=now")
        (let ((after-size (doom-directory-size default-directory)))
          (if (equal after-size before-size)
              (print! (success "\rrepos/%s cannot be compacted further" repo))
            (print! (success "\rRegrafted repos/%s (from %0.1fKB to %0.1fKB)")
                    repo before-size after-size)))))
    t))

(defun doom-packages--regraft-repos (repos)
  (if (not repos)
      (prog1 0
        (print! (item "No repos to regraft")))
    (print! (start "Regrafting %d repos..." (length repos)))
    (let ((before-size (doom-directory-size (straight--repos-dir))))
      (print-group!
       (prog1 (delq nil (mapcar #'doom-packages--regraft-repo repos))
         ;; (princ "\r\033[K")
         (let ((after-size (doom-directory-size (straight--repos-dir))))
           (print! (success "\rFinished regrafting. Size before: %0.1fKB and after: %0.1fKB (%0.1fKB)")
                   before-size after-size
                   (- after-size before-size))))))))

(defun doom-packages--purge-repo (repo)
  (let ((repo-dir (straight--repos-dir repo)))
    (when (file-directory-p repo-dir)
      (delete-directory repo-dir 'recursive)
      (delete-file (straight--modified-file repo))
      (if (file-directory-p repo-dir)
          (ignore (print! (error "Failed to purge repos/%s" repo)))
        (print! (success "Purged repos/%s" repo))
        t))))

(defun doom-packages--purge-repos (repos)
  (if (not repos)
      (prog1 0
        (print! (item "No repos to purge")))
    (print! (start "Purging straight repositories..."))
    (print-group!
     (length
      (delq nil (mapcar #'doom-packages--purge-repo repos))))))

(defun doom-packages--purge-elpa ()
  (let ((dirs (doom-files-in package-user-dir :type t :depth 0)))
    (if (not dirs)
        (prog1 0
          (print! (item "No ELPA packages to purge")))
      (print! (start "Purging ELPA packages..."))
      (dolist (path dirs (length dirs))
        (condition-case e
            (print-group!
             (if (file-directory-p path)
                 (delete-directory path 'recursive)
               (delete-file path))
             (print! (success "Deleted %s") (filename path)))
          (error
           (print! (error "Failed to delete %s because: %s")
                   (filename path)
                   e)))))))

(defun doom-packages--purge-eln ()
  (if-let* ((dirs
             (cl-delete (expand-file-name comp-native-version-dir doom-packages--eln-output-path)
                        (directory-files doom-packages--eln-output-path t "^[^.]" t)
                        :test #'file-equal-p)))
      (progn
        (print! (start "Purging old native bytecode..."))
        (print-group!
         (dolist (dir dirs)
           (print! (item "Deleting %S") (relpath dir doom-packages--eln-output-path))
           (delete-directory dir 'recursive))
         (print! (success "Purged %d directory(ies)" (length dirs))))
        (length dirs))
    (print! (item "No ELN directories to purge"))
    0))

(defun doom-packages-purge (&optional elpa-p builds-p repos-p regraft-repos-p eln-p)
  "Auto-removes orphaned packages and repos.

An orphaned package is a package that isn't a primary package (i.e. doesn't have
a `package!' declaration) or isn't depended on by another primary package.

If BUILDS-P, include straight package builds.
If REPOS-P, include straight repos.
If ELPA-P, include packages installed with package.el (M-x package-install)."
  (doom-initialize-packages)
  (doom-packages--barf-if-incomplete)
  (print! (start "Purging orphaned packages (for the emperor)..."))
  (quiet! (straight-prune-build-cache))
  (cl-destructuring-bind (&optional builds-to-purge repos-to-purge repos-to-regraft)
      (let ((rdirs
             (and (or repos-p regraft-repos-p)
                  (straight--directory-files (straight--repos-dir) nil nil 'sort))))
        (list (when builds-p
                (let ((default-directory (straight--build-dir)))
                  (seq-filter #'file-directory-p
                              (seq-remove (doom-rpartial #'gethash straight--profile-cache)
                                          (straight--directory-files default-directory nil nil 'sort)))))
              (when repos-p
                (seq-remove (doom-rpartial #'straight--checkhash straight--repo-cache)
                            rdirs))
              (when regraft-repos-p
                (seq-filter (doom-rpartial #'straight--checkhash straight--repo-cache)
                            rdirs))))
    (print-group!
     (delq
      nil (list
           (if (not builds-p)
               (ignore (print! (item "Skipping builds")))
             (/= 0 (doom-packages--purge-builds builds-to-purge)))
           (if (not elpa-p)
               (ignore (print! (item "Skipping elpa packages")))
             (/= 0 (doom-packages--purge-elpa)))
           (if (not repos-p)
               (ignore (print! (item "Skipping repos")))
             (/= 0 (doom-packages--purge-repos repos-to-purge)))
           (if (not regraft-repos-p)
               (ignore (print! (item "Skipping regrafting")))
             (doom-packages--regraft-repos repos-to-regraft))
           (when (featurep 'native-compile)
             (if (not eln-p)
                 (ignore (print! (item "Skipping native bytecode")))
               (doom-packages--purge-eln))))))))

(provide 'doom-lib '(packages))
;;; packages.el ends here
