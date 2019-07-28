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

(defcli! (update u) ()
  "Updates packages.

This excludes packages whose `package!' declaration contains a non-nil :freeze
or :ignore property."
  (doom--ensure-autoloads-while
   (straight-check-all)
   (doom-packages-update doom-auto-accept)))

(defcli! (rebuild build b) (&rest args)
  "Rebuilds all installed packages.

This ensures that all needed files are symlinked from their package repo and
their elisp files are byte-compiled."
  (doom--ensure-autoloads-while
   (doom-packages-rebuild doom-auto-accept (member "-f" args))))

(defcli! (purge p) (&rest args)
  "Deletes any unused packages and repos."
  (doom--ensure-autoloads-while
   (straight-check-all)
   (doom-packages-purge 'elpa-p 'build-p
                        (member "-f" args)
                        doom-auto-accept)))

;; (defcli! rollback () ; TODO rollback
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
       (dolist (recipe (hash-table-values straight--recipe-cache))
         (straight--with-plist recipe (package local-repo no-build)
           (unless (or no-build (null local-repo))
             ;; REVIEW We do these modification checks manually because
             ;;        Straight's checks seem to miss stale elc files. Need
             ;;        more tests to confirm this.
             (when (or (ignore-errors
                         (gethash package straight--packages-to-rebuild))
                       (gethash package straight--cached-package-modifications)
                       (not (file-directory-p (straight--build-dir package)))
                       (cl-loop for file
                                in (doom-files-in (straight--build-dir package)
                                                  :match "\\.el$"
                                                  :full t)
                                for elc-file = (byte-compile-dest-file file)
                                if (and (file-exists-p elc-file)
                                        (file-newer-than-file-p file elc-file))
                                return t))
               (let ((straight--packages-to-rebuild :all)
                     (straight--packages-not-to-rebuild (make-hash-table :test #'equal)))
                 (straight-use-package (intern package) nil nil " "))
               (straight--byte-compile-package recipe)
               (cl-incf n))))))
     (if (= n 0)
         (ignore (print! (success "No packages need rebuilding")))
       (doom--finalize-straight)
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
   (let (futures)
     (dolist (group (seq-partition (hash-table-values straight--repo-cache) 8))
       (push
        (async-start
         `(lambda ()
            (setq load-path ',load-path
                  doom-modules ',doom-modules)
            (load ,(concat doom-core-dir "core.el"))
            (let (packages errors)
              (dolist (recipe ',group)
                (straight--with-plist recipe
                    (package local-repo remote upstream-repo upstream-host)
                  ;; HACK There's a contingency of `straight-fetch-package'
                  ;; where it will pop up a window for confirmation, but this
                  ;; window is invisible because a) this command runs in a
                  ;; headless session and b) this code runs in an async child
                  ;; process, so we ensure the remotes are correctly set up to
                  ;; prevent that contingency.
                  (when (and local-repo (straight--repository-is-available-p recipe))
                    (when-let*
                        ((url (ignore-errors (straight--get-call "git" "remote" "get-url" remote)))
                         (desired-url (straight-vc-git--encode-url upstream-repo upstream-host)))
                      (unless (straight-vc-git--urls-compatible-p url desired-url)
                        (straight--get-call "git" "remote" "remove" remote)
                        (straight--get-call "git" "remote" "add" remote desired-url)
                        (straight--get-call "git" "fetch" remote)))
                    (straight-fetch-package package)
                    ;; REVIEW Is there no better way to get this information?
                    (condition-case e
                        (let* ((default-directory (straight--repos-dir local-repo))
                               (n (string-to-number
                                   (straight--get-call "git" "rev-list" "--right-only" "--count" "HEAD..@{u}")))
                               (pretime
                                (string-to-number
                                 (shell-command-to-string "git log -1 --format=%at HEAD")))
                               (time
                                (string-to-number
                                 ;; HACK `straight--get-call' has a higher
                                 ;; failure rate when querying FETCH_HEAD; not
                                 ;; sure why. Doing this manually, with
                                 ;; `shell-command-to-string' works fine.
                                 (shell-command-to-string "git log -1 --format=%at FETCH_HEAD"))))
                          (with-current-buffer (straight--process-get-buffer)
                            (with-silent-modifications
                              (print! (debug (autofill "%s") (indent 2 (buffer-string))))
                              (erase-buffer)))
                          (when (> n 0)
                            (push (list n pretime time recipe)
                                  packages)))
                      (error
                       (push (cons package (string-trim (straight--process-get-output)))
                             errors))))))
              (cons errors (nreverse packages)))))
        futures))
     (condition-case e
         (let ((total (length futures))
               (futures (nreverse futures))
               (specs '(t)))
           (while futures
             (print! ". %.0f%%" (* (/ (- total (length futures))
                                      (float total))
                                   100))
             (while (not (async-ready (car futures)))
               (sleep-for 2)
               (print! "."))
             (cl-destructuring-bind (errors . packages)
                 (async-get (pop futures))
               (if errors
                   (error "There were errors:\n\n%s"
                          (mapconcat (lambda (e)
                                       (format! " - %s: %s" (yellow (car e)) (cdr e)))
                                     errors
                                     "\n")
                          errors)
                 (nconc specs packages))))
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
                 (straight--make-package-modifications-available)
                 (let ((straight--packages-to-rebuild (make-hash-table :test #'equal))
                       (straight--packages-not-to-rebuild (make-hash-table :test #'equal)))
                   (dolist (spec specs)
                     (cl-destructuring-bind (n pretime time recipe) spec
                       (straight--with-plist recipe (local-repo package)
                         (let ((default-directory (straight--repos-dir local-repo)))
                           (print! (start "Updating %S") package)
                           ;; HACK `straight' assumes it won't be used in a
                           ;; noninteractive session, but here we are. If the repo
                           ;; is dirty, the command will lock up, waiting for
                           ;; interaction that will never come, so discard all
                           ;; local changes. Doom doesn't want you modifying those
                           ;; anyway.
                           (and (straight--get-call "git" "reset" "--hard")
                                (straight--get-call "git" "clean" "-ffd"))
                           (straight-merge-package package)
                           ;; HACK `straight-rebuild-package' doesn't pick up that
                           ;; this package has changed, so we do it manually. Is
                           ;; there a better way?
                           (ignore-errors
                             (delete-directory (straight--build-dir package) 'recursive))
                           (puthash package t straight--packages-to-rebuild)
                           (cl-incf n))
                         (with-current-buffer (straight--process-get-buffer)
                           (with-silent-modifications
                             (print! (debug (autofill "%s") (indent 2 (buffer-string))))
                             (erase-buffer))))))
                   (doom--finalize-straight)
                   (doom-packages-rebuild auto-accept-p))
                 t)
             (print! (success "No packages to update"))
             nil))
       (error
        (message "Output:\n%s" (straight--process-get-output))
        (signal (car e) (error-message-string e)))))))


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

(defmacro doom--packages-purge (packages label auto-accept-p &rest files)
  (declare (indent defun))
  `(let ((packages ,packages)
         (label ,label))
     (if (not packages)
         (ignore (print! (success "No orphaned %s(s) to purge" label)))
       (if (not (or ,auto-accept-p
                    (y-or-n-p
                     (format! "\n%s\n\n%d %s(s) are orphaned. Purge them (for the Emperor)?"
                              (mapconcat (lambda (pkgs)
                                           (mapconcat (lambda (p) (format "  + %-20.20s" p))
                                                      pkgs
                                                      ""))
                                         (seq-partition (cl-sort (copy-sequence packages) #'string-lessp)
                                                        3)
                                         "\n")
                              (length packages)
                              label))))
           (ignore (print! (warn "Aborted")))
         (let ((n 0))
           (print! (start "Pruning %ss..." label))
           (print-group!
            (dolist (it packages)
              (print! (info "Deleting %s/%s") label it)
              (dolist (path (list ,@files))
                (cond ((file-directory-p path)
                       (delete-directory path 'recursive))
                      ((file-regular-p path)
                       (delete-file path)))
                (if (file-exists-p path)
                    (print! (error "Failed to find %s/%s") label it)
                  (cl-incf n))))
            (if (= n 0)
                (ignore (print! (warn "Didn't prune any %s(s) for some reason" label)))
              (print! (success "Pruned %d %s(s)" n label))
              (doom--finalize-straight)
              t)))))))

(defun doom-packages-purge (&optional elpa-p builds-p repos-p auto-accept-p)
  "Auto-removes orphaned packages and repos.

An orphaned package is a package that isn't a primary package (i.e. doesn't have
a `package!' declaration) or isn't depended on by another primary package.

If BUILDS-P, include straight package builds.
If REPOS-P, include straight repos.
If ELPA-P, include packages installed with package.el (M-x package-install).

Unless AUTO-ACCEPT-P is non-nil, this function will prompt for confirmation with
a list of packages that will be removed."
  (print! (start "Searching for orphaned packages..."))
  (cl-destructuring-bind (builds repos)
      (doom--packages-to-purge)
    (let (success)
      (print-group!
       (if builds-p
           (and (doom--packages-purge builds "build" auto-accept-p
                  (straight--build-dir it)
                  (straight--modified-file it))
                (setq success t)
                (straight-prune-build-cache))
         (print! (info "Skipping builds")))
       (if repos-p
           (and (doom--packages-purge repos "repo" auto-accept-p
                  (straight--repos-dir it))
                (setq success t))
         (print! (info "Skipping repos")))
       (if (not elpa-p)
           (print! (info "Skipping elpa packages"))
         (unless (bound-and-true-p package--initialized)
           (package-initialize))
         (and (doom--packages-purge (mapcar #'symbol-name (mapcar #'car package-alist))
                "package" auto-accept-p
                (package-desc-dir (cadr (assq (intern it) package-alist))))
              (setq success t))
         (when (file-directory-p package-user-dir)
           (delete-directory package-user-dir t)))
       (when success
         (doom--finalize-straight)
         t)))))
