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

(defcli! (update u) (&rest args)
  "Updates packages.

This works by fetching all installed package repos and checking the distance
between HEAD and FETCH_HEAD. This can take a while.

This excludes packages whose `package!' declaration contains a non-nil :freeze
or :ignore property."
  (doom--ensure-autoloads-while
   (straight-check-all)
   (doom-packages-update
    doom-auto-accept
    (when-let (timeout (cadr (or (member "--timeout" args)
                                 (member "-t" args))))
      (string-to-number timeout)))))

(defcli! (rebuild build b) (&rest args)
  "Rebuilds all installed packages.

This ensures that all needed files are symlinked from their package repo and
their elisp files are byte-compiled."
  (doom--ensure-autoloads-while
   (doom-packages-rebuild doom-auto-accept (member "-f" args))))

(defcli! (purge p) (&rest args)
  "Deletes any unused ELPA packages, straight builds, and (optionally) repos.

By default, this does not purge repos.

Available options:

--no-elpa    Don't purge ELPA packages
--no-builds  Don't purge unneeded (built) packages
--repos      Purge unused repos"
  (doom--ensure-autoloads-while
   (straight-check-all)
   (doom-packages-purge (not (member "--no-elpa" args))
                        (not (member "--no-builds" args))
                        (or (member "-r" args)
                            (member "--repos" args))
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
               (let ((straight-use-package-pre-build-functions
                      straight-use-package-pre-build-functions))
                 (add-hook 'straight-use-package-pre-build-functions
                           (lambda (&rest _) (cl-incf n)))
                 (let ((straight--packages-to-rebuild :all)
                       (straight--packages-not-to-rebuild (make-hash-table :test #'equal)))
                   (straight-use-package (intern package) nil nil " "))
                 (straight--byte-compile-package recipe)
                 (dolist (dep (straight--get-dependencies package))
                   (when-let (recipe (gethash dep straight--recipe-cache))
                     (straight--byte-compile-package recipe)))))))))
     (if (= n 0)
         (ignore (print! (success "No packages need rebuilding")))
       (doom--finalize-straight)
       (print! (success "Rebuilt %d package(s)" n))
       t))))


(defun doom--packages-remove-outdated-f (packages)
  (async-start
   `(lambda ()
      (setq load-path ',load-path
            doom-modules ',doom-modules
            user-emacs-directory ',user-emacs-directory)
      (condition-case e
          (let (packages errors)
            (load ,(concat doom-core-dir "core.el"))
            (doom-initialize 'force-p)
            (dolist (recipe ',group)
              (when (straight--repository-is-available-p recipe)
                (straight-vc-git--destructure recipe
                    (package local-repo nonrecursive upstream-remote upstream-repo upstream-host
                             branch remote)
                  (condition-case e
                      (let ((default-directory (straight--repos-dir local-repo)))
                        ;; HACK We normalize packages to avoid certain scenarios
                        ;; where `straight-fetch-package' will create an
                        ;; interactive popup prompting for action (which will
                        ;; cause this async process to block indefinitely). We
                        ;; can't use `straight-normalize-package' because could
                        ;; create popup prompts too, so we do it manually:
                        (shell-command-to-string "git merge --abort")
                        (straight--get-call "git" "reset" "--hard" (format "%s/%s" remote branch))
                        (straight--get-call "git" "clean" "-ffd")
                        (unless nonrecursive
                          (shell-command-to-string "git submodule update --init --recursive"))
                        (when upstream-repo
                          (let ((desired-url (straight-vc-git--encode-url upstream-repo upstream-host))
                                (actual-url (condition-case nil
                                                (straight--get-call "git" "remote" "get-url" upstream-remote)
                                              (error nil))))
                            (unless (straight-vc-git--urls-compatible-p actual-url desired-url)
                              (straight--get-call "git" "remote" "remove" upstream-remote)
                              (straight--get-call "git" "remote" "add" upstream-remote desired-url)
                              (straight--get-call "git" "fetch" upstream-remote))))
                        (straight-fetch-package package)
                        ;; REVIEW Is there no better way to get this information?
                        (let ((n (length
                                  (split-string
                                   (straight--get-call "git" "rev-list" "--left-right" "HEAD..@{u}")
                                   "\n" t)))
                              (pretime
                               (string-to-number
                                (shell-command-to-string "git log -1 --format=%at HEAD")))
                              (time
                               (string-to-number
                                ;; HACK `straight--get-call' has a higher failure
                                ;; rate when querying FETCH_HEAD; not sure why.
                                ;; Doing this manually, with
                                ;; `shell-command-to-string' works fine.
                                (shell-command-to-string "git log -1 --format=%at FETCH_HEAD"))))
                          (with-current-buffer (straight--process-get-buffer)
                            (with-silent-modifications
                              (print! (debug (autofill "%s") (indent 2 (buffer-string))))
                              (erase-buffer)))
                          (when (> n 0)
                            (push (list n pretime time recipe)
                                  packages))))
                    (error
                     (push (list package e (string-trim (or (straight--process-get-output) "")))
                           errors))))))
            (if errors
                (cons 'error errors)
              (cons 'ok (nreverse packages))))
        (error
         (cons 'error e))))))


(defun doom-packages-update (&optional auto-accept-p timeout)
  "Updates packages.

Unless AUTO-ACCEPT-P is non-nil, this function will prompt for confirmation with
a list of packages that will be updated."
  (print! (start "Scanning for outdated packages (this may take a while)..."))
  (print-group!
   (when timeout
     (print! (info "Using %S as timeout value" timeout)))
   ;; REVIEW Does this fail gracefully enough? Is it error tolerant?
   ;; TODO Add version-lock checks; don't want to spend all this effort on
   ;;      packages that shouldn't be updated
   (let* ((futures
           (or (cl-loop for group
                        in (seq-partition (hash-table-values straight--repo-cache)
                                          (/ (hash-table-count straight--repo-cache)
                                             16))
                        for future = (doom--packages-remove-outdated-f group)
                        if (processp future)
                        collect (cons future group)
                        else
                        do (print! (warn "Failed to create thread for:\n\n%s\n\nReason: %s"
                                         group future)))
               (error! "Failed to create any threads")))
          (total (length futures))
          (timeout (or timeout 45)))
     (condition-case-unless-debug e
         (let (specs)
           (while futures
             (print! ". %.0f%%" (* (/ (- total (length futures))
                                      (float total))
                                   100))
             (let ((time 0))
               (catch 'timeout
                 (while (not (async-ready (caar futures)))
                   (when (> time timeout)
                     (print! (warn "A thread has timed out. The following packages were skipped: %s"
                                   (mapconcat (lambda (p) (plist-get p :package))
                                              (cdar futures)
                                              ", ")))
                     (throw 'timeout (pop futures)))
                   (sleep-for 1)
                   (when (cl-evenp time)
                     (print! "."))
                   (cl-incf time))
                 (cl-destructuring-bind (status . result)
                     (or (async-get (car (pop futures)))
                         (cons nil nil))
                   (cond ((null status)
                          (error "Thread returned an invalid result: %S" errors))
                         ((eq status 'error)
                          (error "There were errors:\n\n%s"
                                 (cond ((and (listp result)
                                             (symbolp (car result)))
                                        (prin1-to-string result))
                                       ((stringp result)
                                        result)
                                       ((mapconcat (lambda (e)
                                                     (format! " - %s: %s" (yellow (car e)) (cdr e)))
                                                   result
                                                   "\n")))))
                         ((eq status 'ok)
                          (print! (debug "Appended %S to package list") (or result "nothing"))
                          (appendq! specs result))
                         ((error "Thread returned a non-standard status: %s\n\n%s"
                                 status result)))))))
           (print! ". 100%%")
           (terpri)
           (if-let (specs (delq nil specs))
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


;;; PURGE (for the emperor)
(defun doom--prompt-p (list-fn list preamble postamble)
  (or (y-or-n-p (format "%s%s\n\n%s"
                        (if preamble (concat preamble "\n\n") "")
                        (mapconcat list-fn list "\n")
                        (or postamble "")))
      (user-error! "Aborted")))

(defun doom--prompt-columns-p (row-fn list preamble postamble)
  (doom--prompt-p (lambda (row)
                    (mapconcat row-fn row ""))
                  (seq-partition (cl-sort (copy-sequence list) #'string-lessp)
                                 3)
                  preamble
                  postamble))

(defun doom--packages-purge-build (build)
  (let ((build-dir (straight--build-dir build)))
    (print! (start "Purging build/%s..." build))
    (delete-directory build-dir 'recursive)
    (if (file-directory-p build-dir)
        (ignore (print! (error "Failed to purg build/%s" build)))
      (print! (success "Purged build/%s" build))
      t)))

(defun doom--packages-purge-builds (builds &optional auto-accept-p)
  (if (not builds)
      (progn (print! (info "No builds to purge"))
             0)
    (or auto-accept-p
        (doom--prompt-columns-p
         (lambda (p) (format "  + %-20.20s" p)) builds nil
         (format! "Found %d orphaned package builds. Purge them?"
                  (length builds))))
    (length
     (delq nil (mapcar #'doom--packages-purge-build builds)))))

(defun doom--packages-regraft-repo (repo)
  (let ((default-directory (straight--repos-dir repo)))
    (if (not (file-directory-p ".git"))
        (ignore (print! (warn "repos/%s is not a git repo, skipping" repo)))
      (print! (debug "Regrafting repos/%s..." repo))
      (straight--call "git" "reset" "--hard")
      (straight--call "git" "clean" "--ffd")
      (straight--call "git" "replace" "--graft" "HEAD")
      (straight--call "git" "gc")
      (print! (debug "%s" (straight--process-get-output)))
      (print! (success "Regrafted repos/%s" repo))
      t)))

(defun doom--packages-regraft-repos (repos &optional auto-accept-p)
  (if (not repos)
      (progn (print! (info "No repos to regraft"))
             0)
    (or auto-accept-p
        (y-or-n-p (format! "Preparing to regraft all %d repos. Continue?"
                           (length repos)))
        (user-error! "Aborted!"))
    (if (executable-find "du")
        (cl-destructuring-bind (status . size)
            (doom-sh "du" "-sh" (straight--repos-dir))
          (prog1 (delq nil (mapcar #'doom--packages-regraft-repo repos))
            (cl-destructuring-bind (status . newsize)
                (doom-sh "du" "-sh" (straight--repos-dir))
              (print! (success "Finshed regrafted. Size before: %s and after: %s"
                               (car (split-string size "\t"))
                               (car (split-string newsize "\t")))))))
        (delq nil (mapcar #'doom--packages-regraft-repo repos)))))

(defun doom--packages-purge-repo (repo)
  (print! (debug "Purging repos/%s..." repo))
  (let ((repo-dir (straight--repos-dir repo)))
    (delete-directory repo-dir 'recursive)
    (ignore-errors
      (delete-file (straight--modified-file repo)))
    (if (file-directory-p repo-dir)
        (ignore (print! (error "Failed to purge repos/%s" repo)))
      (print! (success "Purged repos/%s" repo))
      t)))

(defun doom--packages-purge-repos (repos &optional auto-accept-p)
  (if (not repos)
      (progn (print! (info "No repos to purge"))
             0)
    (or auto-accept-p
        (doom--prompt-columns-p
         (lambda (p) (format "  + %-20.20s" p)) repos nil
         (format! "Found %d orphaned repos. Purge them?"
                  (length repos))))
    (length
     (delq nil (mapcar #'doom--packages-purge-repo repos)))))

(defun doom--packages-purge-elpa (&optional auto-accept-p)
  (unless (bound-and-true-p package--initialized)
    (package-initialize))
  (if (not package-alist)
      (progn (print! (info "No ELPA packages to purge"))
             0)
    (doom--prompt-columns-p
     (lambda (p) (format "  + %-20.20s" p))
     (mapcar #'car package-alist) nil
     (format! "Found %d orphaned ELPA packages. Purge them?"
              (length package-alist)))
    (mapc (doom-rpartial #'delete-directory 'recursive)
          (mapcar #'package-desc-dir
                  (mapcar #'cadr package-alist)))
    (length package-alist)))

(defun doom-packages-purge (&optional elpa-p builds-p repos-p auto-accept-p)
  "Auto-removes orphaned packages and repos.

An orphaned package is a package that isn't a primary package (i.e. doesn't have
a `package!' declaration) or isn't depended on by another primary package.

If BUILDS-P, include straight package builds.
If REPOS-P, include straight repos.
If ELPA-P, include packages installed with package.el (M-x package-install).

Unless AUTO-ACCEPT-P is non-nil, this function will prompt for confirmation with
a list of packages that will be removed."
  (print! (start "Searching for orphaned packages to purge (for the emperor)..."))
  (cl-destructuring-bind (&optional builds-to-purge repos-to-purge repos-to-regraft)
      (let ((rdirs (straight--directory-files (straight--repos-dir) nil nil 'sort))
            (bdirs (straight--directory-files (straight--build-dir) nil nil 'sort)))
        (list (cl-remove-if (doom-rpartial #'gethash straight--profile-cache)
                            bdirs)
              (cl-remove-if (doom-rpartial #'straight--checkhash straight--repo-cache)
                            rdirs)
              (cl-remove-if-not (doom-rpartial #'straight--checkhash straight--repo-cache)
                                rdirs)))
    (let (success)
      (print-group!
       (if (not builds-p)
           (print! (info "Skipping builds"))
         (and (/= 0 (doom--packages-purge-builds builds-to-purge auto-accept-p))
              (setq success t)
              (straight-prune-build-cache)))
       (if (not elpa-p)
           (print! (info "Skipping elpa packages"))
         (and (/= 0 (doom--packages-purge-elpa auto-accept-p))
              (setq success t)))
       (if (not repos-p)
           (print! (info "Skipping repos"))
         (and (/= 0 (doom--packages-purge-repos repos-to-purge auto-accept-p))
              (setq success t))
         (and (doom--packages-regraft-repos repos-to-regraft auto-accept-p)
              (setq success t)))
       (when success
         (doom--finalize-straight)
         t)))))
