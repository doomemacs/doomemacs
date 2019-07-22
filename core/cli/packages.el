;; -*- no-byte-compile: t; -*-
;;; core/cli/packages.el

(defmacro doom--ensure-autoloads-while (&rest body)
  `(progn
     (doom-reload-core-autoloads)
     (when (progn ,@body)
       (doom-reload-package-autoloads 'force-p))
     t))


;;
;;; Dispatchers

(def-command! (update u) ()
  "Updates packages.

This excludes packages whose `package!' declaration contains a non-nil :freeze
or :ignore property."
  (doom--ensure-autoloads-while
   (straight-check-all)
   (when (doom-packages-update doom-auto-accept)
     (doom-packages-rebuild doom-auto-accept)
     t)))

(def-command! (rebuild b) (&rest args)
  "Rebuilds all installed packages.

This ensures that all needed files are symlinked from their package repo and
their elisp files are byte-compiled."
  (doom--ensure-autoloads-while
   (doom-packages-rebuild doom-auto-accept (member "all" args))))

(def-command! (purge p) ()
  "Deletes any unused packages and package repos.

You should run this once in a while, as repos tend to build up over time."
  (doom--ensure-autoloads-while
   (straight-check-all)
   (doom-packages-purge doom-auto-accept)))

;; (def-command! rollback () ; TODO rollback
;;   "<Not implemented yet>"
;;   (user-error "Not implemented yet, sorry!"))


;;
;;; Library

(defun doom-packages-install (&optional auto-accept-p)
  "Installs missing packages.

This function will install any primary package (i.e. a package with a `package!'
declaration) or dependency thereof that hasn't already been.

Unless AUTO-ACCEPT-P is non-nil, this function will prompt for confirmation with
a list of packages that will be installed."
  (print! "> Installing & building packages...")
  (print-group!
   (let ((n 0))
     (dolist (package (hash-table-keys straight--recipe-cache))
       (straight--with-plist (gethash package straight--recipe-cache)
           (local-repo)
         (let ((existed-p (file-directory-p (straight--repos-dir package))))
           (condition-case-unless-debug e
               (and (straight-use-package (intern package) nil nil " ")
                    (not existed-p)
                    (file-directory-p (straight--repos-dir package))
                    (cl-incf n))
             (error
              (signal 'doom-package-error
                      (list e (straight--process-get-output))))))))
     (if (= n 0)
         (ignore (print! (success "No packages need to be installed")))
       (print! (success "Installed & built %d packages") n)
       t))))


(defun doom-packages-rebuild (&optional auto-accept-p all)
  "(Re)build all packages."
  (print! (start "(Re)building %spackages...") (if all "all " ""))
  (print-group!
   (let ((n 0))
     (if all
         (let ((straight--packages-to-rebuild :all)
               (straight--packages-not-to-rebuild (make-hash-table :test #'equal)))
           (dolist (package (hash-table-keys straight--recipe-cache))
             (straight-use-package
              (intern package) nil (lambda (_) (cl-incf n) nil) " ")))
       (let ((straight-check-for-modifications '(find-when-checking)))
         (straight-check-all)
         (dolist (recipe (hash-table-values straight--recipe-cache))
           (straight--with-plist recipe (package local-repo no-build)
             (unless (or no-build (null local-repo))
               ;; REVIEW We do these modification checks manually because
               ;;        Straight's checks seem to miss stale elc files. Need
               ;;        more tests to confirm this.
               (when (or (gethash package straight--cached-package-modifications)
                         (file-newer-than-file-p (straight--repos-dir local-repo)
                                                 (straight--build-dir package))
                         (cl-loop for file
                                  in (doom-files-in (straight--build-dir package)
                                                    :match "\\.el$"
                                                    :full t)
                                  for elc-file = (byte-compile-dest-file file)
                                  if (and (file-exists-p elc-file)
                                          (file-newer-than-file-p file elc-file))
                                  return t))
                 (print! (info "Rebuilding %s") package)
                 ;; REVIEW `straight-rebuild-package' alone wasn't enough. Why?
                 (delete-directory (straight--build-dir package) 'recursive)
                 (straight-rebuild-package package)
                 (cl-incf n)))))))
     (if (= n 0)
         (ignore (print! (success "No packages need rebuilding")))
       (print! (success "Rebuilt %d package(s)" n))
       t))))


(defun doom-packages-update (&optional auto-accept-p)
  "Updates packages.

Unless AUTO-ACCEPT-P is non-nil, this function will prompt for confirmation with
a list of packages that will be updated."
  (print! (start "Scanning for outdated packages (this may take a while)..."))
  (print-group!
   ;; REVIEW Does this fail gracefully enough? Is it error tolerant?
   ;; TODO Add version-lock checks; don't want to spend all this effort on
   ;;      packages that shouldn't be updated
   (condition-case e
       (let (futures)
         (dolist (group (seq-partition (hash-table-values straight--repo-cache) 8))
           (push (async-start
                  `(lambda ()
                     (setq load-path ',load-path
                           doom-modules ',doom-modules)
                     (load ,(concat doom-core-dir "core.el"))
                     (let (packages)
                       (when (require 'straight nil t)
                         (dolist (recipe ',group)
                           (straight--with-plist recipe (package local-repo)
                             (when (and local-repo (straight--repository-is-available-p recipe))
                               (straight-fetch-package package)
                               ;; REVIEW Isn't there a better way to get this information? Maybe with `vc'?
                               (let* ((default-directory (straight--repos-dir local-repo))
                                      (n (string-to-number
                                          (shell-command-to-string "git rev-list --right-only --count HEAD..@'{u}'")))
                                      (pretime
                                       (string-to-number
                                        (shell-command-to-string "git log -1 --format=%at HEAD")))
                                      (time
                                       (string-to-number
                                        (shell-command-to-string "git log -1 --format=%at FETCH_HEAD"))))
                                 (when (> n 0)
                                   (push (list n pretime time recipe)
                                         packages)))))))
                       (nreverse packages))))
                 futures))
         (let ((total (length futures))
               (futures (nreverse futures))
               (specs '(t)))
           (while futures
             (while (not (async-ready (car futures)))
               (sleep-for 2)
               (print! "."))
             (nconc specs (async-get (pop futures))))
           (terpri)
           (if-let (specs (delq nil (cdr specs)))
               (if (not
                    (or auto-accept-p
                        (y-or-n-p
                         (format!
                          "%s\n\nThere %s %d package%s available to update. Update them?"
                          (mapconcat
                           (lambda (spec)
                             (cl-destructuring-bind (n pretime time recipe) spec
                               (straight--with-plist recipe (package)
                                 (format! "+ %-33s %s commit(s) behind   %s -> %s"
                                          (yellow package) (yellow n)
                                          (format-time-string "%Y%m%d" pretime)
                                          (format-time-string "%Y%m%d" time)))))
                           specs
                           "\n")
                          (if (cdr specs) "are" "is")
                          (length specs)
                          (if (cdr specs) "s" "")))))
                   (ignore (print! (info "Aborted update")))
                 (terpri)
                 (dolist (spec specs t)
                   (cl-destructuring-bind (n pretime time recipe) spec
                     (straight--with-plist recipe (local-repo package)
                       (let ((default-directory (straight--repos-dir local-repo)))
                         (print! (start "Updating %S") package)
                         ;; HACK `straight' doesn't assume it would ever be used
                         ;; non-interactively, but here we are. If the repo is
                         ;; dirty, the command will lock up, waiting for
                         ;; interaction that will never come, so discard all local
                         ;; changes. Doom doesn't want you modifying those anyway.
                         (and (straight--get-call "git" "reset" "--hard")
                              (straight--get-call "git" "clean" "-ffd"))
                         (straight-merge-package package)
                         ;; HACK `straight-rebuild-package' doesn't pick up that
                         ;; this package has changed, so we do it manually. Is
                         ;; there a better way?
                         (run-hook-with-args 'straight-use-package-pre-build-functions package)
                         (straight--build-package recipe "   "))
                       (with-current-buffer (straight--process-get-buffer)
                         (with-silent-modifications
                           (erase-buffer)))))))
             (print! (success "No packages to update"))
             nil)))
     (error
      (message "Output:\n%s" (straight--process-get-output))
      (signal (car e) (error-message-string e))))))


(defun doom--packages-to-purge ()
  (let (builds repos)
    (dolist (name (straight--directory-files (straight--repos-dir)))
      (unless (straight--checkhash name straight--repo-cache)
        (push name repos)))
    (dolist (name (straight--directory-files (straight--build-dir)))
      (unless (gethash name straight--profile-cache)
        (push name builds)))
    (straight-prune-build-cache)
    (list builds repos)))

(defun doom-packages-purge (&optional auto-accept-p)
  "Auto-removes orphaned packages and repos.

An orphaned package is a package that isn't a primary package (i.e. doesn't have
a `package!' declaration) or isn't depended on by another primary package.

Unless AUTO-ACCEPT-P is non-nil, this function will prompt for confirmation with
a list of packages that will be removed."
  (print! (start "Searching for orphaned packages..."))
  (cl-destructuring-bind (builds repos) (doom--packages-to-purge)
    (unless (bound-and-true-p package--initialized)
      (package-initialize))
    (print-group!
     (let ((packages (append builds (mapcar #'car package-alist) nil)))
       (if (not packages)
           (ignore (print! (success "No orphaned packages to purge")))
         (or auto-accept-p
             (y-or-n-p
              (format! "\n%s\n\n%d packages are orphaned. Purge them (for the Emperor)?"
                       (mapconcat (lambda (pkgs)
                                    (mapconcat (lambda (p) (format "  + %-20.20s" p))
                                               pkgs
                                               ""))
                                  (seq-partition (cl-sort (copy-sequence packages) #'string-lessp)
                                                 3)
                                  "\n")
                       (length packages)))
             (user-error "Aborted"))
         (let ((n 0))
           (dolist (dir (append (mapcar #'straight--repos-dir repos)
                                (mapcar #'straight--build-dir builds)))
             (print! (info "Deleting %S") (relpath dir (straight--dir)))
             (delete-directory dir 'recursive)
             (unless (file-directory-p dir)
               (cl-incf n)))
           (straight-prune-build-cache)
           (when (file-directory-p package-user-dir)
             (delete-directory package-user-dir t)
             t)
           (> n 0)))))))
