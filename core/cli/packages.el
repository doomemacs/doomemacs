;; -*- no-byte-compile: t; -*-
;;; core/cli/packages.el

(defcli! (update u) ()
  "Updates packages.

This works by fetching all installed package repos and checking the distance
between HEAD and FETCH_HEAD. This can take a while.

This excludes packages whose `package!' declaration contains a non-nil :freeze
or :ignore property."
  (straight-check-all)
  (doom-cli-reload-core-autoloads)
  (when (doom-cli-packages-update)
    (doom-cli-reload-package-autoloads 'force-p))
  t)

(defcli! (build b)
    ((rebuild-p ["-r"] "Only rebuild packages that need rebuilding"))
  "Byte-compiles & symlinks installed packages.

This ensures that all needed files are symlinked from their package repo and
their elisp files are byte-compiled. This is especially necessary if you upgrade
Emacs (as byte-code is generally not forward-compatible)."
  (when (doom-cli-packages-build (not rebuild-p))
    (doom-cli-reload-package-autoloads 'force-p))
  t)

(defcli! (purge p)
    ((nobuilds-p ["-b" "--no-builds"] "Don't purge unneeded (built) packages")
     (noelpa-p   ["-p" "--no-elpa"]   "Don't purge ELPA packages")
     (norepos-p  ["-r" "--no-repos"]  "Don't purge unused straight repos")
     (regraft-p  ["-g" "--regraft"]   "Regraft git repos (ie. compact them)"))
  "Deletes orphaned packages & repos, and compacts them.

Purges all installed ELPA packages (as they are considered temporary). Purges
all orphaned package repos and builds. If -g/--regraft is supplied, the git
repos among them will be regrafted and compacted to ensure they are as small as
possible.

It is a good idea to occasionally run this doom purge -g to ensure your package
list remains lean."
  (straight-check-all)
  (when (doom-cli-packages-purge
         (not noelpa-p)
         (not norepos-p)
         (not nobuilds-p)
         regraft-p)
    (doom-cli-reload-package-autoloads 'force-p))
  t)

;; (defcli! rollback () ; TODO doom rollback
;;   "<Not implemented yet>"
;;   (user-error "Not implemented yet, sorry!"))


;;
;;; Library

(defun doom-cli-packages-install ()
  "Installs missing packages.

This function will install any primary package (i.e. a package with a `package!'
declaration) or dependency thereof that hasn't already been."
  (print! (start "Installing & building packages..."))
  (print-group!
   (let ((n 0))
     (dolist (package (hash-table-keys straight--recipe-cache))
       (straight--with-plist (gethash package straight--recipe-cache)
           (local-repo)
         (let ((existed-p (file-directory-p (straight--repos-dir package))))
           (condition-case-unless-debug e
               (and (straight-use-package (intern package) nil nil (make-string (1- (or doom-format-indent 1)) 32))
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


(defun doom-cli-packages-build (&optional force-p)
  "(Re)build all packages."
  (print! (start "(Re)building %spackages...") (if force-p "all " ""))
  (print-group!
   (let ((n 0))
     (if force-p
         (let ((straight--packages-to-rebuild :all)
               (straight--packages-not-to-rebuild (make-hash-table :test #'equal)))
           (dolist (package (hash-table-keys straight--recipe-cache))
             (straight-use-package
              (intern package) nil (lambda (_) (cl-incf n) nil)
              (make-string (1- (or doom-format-indent 1)) 32))))
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
                   (straight-use-package
                    (intern package) nil nil
                    (make-string (or doom-format-indent 0) 32)))
                 (straight--byte-compile-package recipe)
                 (dolist (dep (straight--get-dependencies package))
                   (when-let (recipe (gethash dep straight--recipe-cache))
                     (straight--byte-compile-package recipe)))))))))
     (if (= n 0)
         (ignore (print! (success "No packages need rebuilding")))
       (doom--finalize-straight)
       (print! (success "Rebuilt %d package(s)" n))
       t))))


(defun doom-cli-packages-update ()
  "Updates packages."
  (print! (start "Updating packages (this may take a while)..."))
  ;; TODO Refactor me
  (let ((straight--repos-dir (straight--repos-dir))
        (straight--packages-to-rebuild (make-hash-table :test #'equal))
        (total (hash-table-count straight--repo-cache))
        (i 1)
        errors)
    (print-group!
     (dolist (recipe (hash-table-values straight--repo-cache))
       (straight--with-plist recipe (package type local-repo)
         (condition-case-unless-debug e
             (let ((default-directory (straight--repos-dir local-repo)))
               (if (not (file-in-directory-p default-directory straight--repos-dir))
                   (print! (warn "[%d/%d] Skipping %s because it is local")
                           i total package)
                 (let ((commit (straight-vc-get-commit type local-repo)))
                   (if (not (straight-vc-fetch-from-remote recipe))
                       (print! (warn "\033[K(%d/%d) Failed to fetch %s" i total package))
                     (let ((output (straight--process-get-output)))
                       (straight-merge-package package)
                       (let ((newcommit (straight-vc-get-commit type local-repo)))
                         (if (string= commit newcommit)
                             (print! (start "\033[K(%d/%d) %s is up-to-date\033[1A") i total package)
                           (ignore-errors
                             (delete-directory (straight--build-dir package) 'recursive))
                           (puthash package t straight--packages-to-rebuild)
                           (print! (info "\033[K(%d/%d) Updating %s...") i total package)
                           (unless (string-empty-p output)
                             (print-group!
                              (print! (info "%s") output)
                              (when (eq type 'git)
                                (straight--call "git" "log" "--oneline" newcommit (concat "^" commit))
                                (print-group!
                                 (print! "%s" (straight--process-get-output))))))
                           (print! (success "(%d/%d) %s updated (%s -> %s)") i total package
                                   (substring commit 0 7)
                                   (substring newcommit 0 7))))))))
               (cl-incf i))
           (user-error
            (signal 'user-error (error-message-string e)))
           (error
            (print! (warn "(%d/%d) Encountered error with %s" i total package))
            (print-group!
             (print! (error "%s" e))
             (print-group! (print! (info "%s" (straight--process-get-output)))))
            (push package errors)))))
     (princ "\033[K")
     (when errors
       (print! (error "There were %d errors, the offending packages are: %s")
               (length errors) (string-join errors ", ")))
     (if (hash-table-empty-p straight--packages-to-rebuild)
         (ignore
          (print! (success "All %d packages are up-to-date")
                  (hash-table-count straight--repo-cache)))
       (let ((count (hash-table-count straight--packages-to-rebuild))
             (packages (hash-table-keys straight--packages-to-rebuild)))
         (sort packages #'string-lessp)
         (doom--finalize-straight)
         (doom-cli-packages-build)
         (print! (success "Updated %d package(s)") count))
       t))))


;;; PURGE (for the emperor)
(defun doom--cli-packages-purge-build (build)
  (let ((build-dir (straight--build-dir build)))
    (delete-directory build-dir 'recursive)
    (if (file-directory-p build-dir)
        (ignore (print! (error "Failed to purg build/%s" build)))
      (print! (success "Purged build/%s" build))
      t)))

(defun doom--cli-packages-purge-builds (builds)
  (if (not builds)
      (progn (print! (info "No builds to purge"))
             0)
    (length
     (delq nil (mapcar #'doom--cli-packages-purge-build builds)))))

(defun doom--cli-packages-regraft-repo (repo)
  (let ((default-directory (straight--repos-dir repo)))
    (if (not (file-directory-p ".git"))
        (ignore (print! (warn "\033[Krepos/%s is not a git repo, skipping" repo)))
      (let ((before-size (doom-directory-size default-directory)))
        (straight--call "git" "reset" "--hard")
        (straight--call "git" "clean" "-ffd")
        (if (not (car (straight--call "git" "replace" "--graft" "HEAD")))
            (print! (info "\033[Krepos/%s is already compact\033[1A" repo))
          (straight--call "git" "gc")
          (print! (success "\033[KRegrafted repos/%s (from %0.1fKB to %0.1fKB)")
                  repo before-size (doom-directory-size default-directory))
          (print-group! (print! "%s" (straight--process-get-output)))))
      t)))

(defun doom--cli-packages-regraft-repos (repos)
  (if (not repos)
      (progn (print! (info "No repos to regraft"))
             0)
    (let ((before-size (doom-directory-size (straight--repos-dir))))
      (print-group!
       (prog1 (delq nil (mapcar #'doom--cli-packages-regraft-repo repos))
         (princ "\033[K")
         (let ((after-size (doom-directory-size (straight--repos-dir))))
           (print! (success "Finished regrafting. Size before: %0.1fKB and after: %0.1fKB (%0.1fKB)")
                   before-size after-size
                   (- after-size before-size))))))))

(defun doom--cli-packages-purge-repo (repo)
  (let ((repo-dir (straight--repos-dir repo)))
    (delete-directory repo-dir 'recursive)
    (ignore-errors
      (delete-file (straight--modified-file repo)))
    (if (file-directory-p repo-dir)
        (ignore (print! (error "Failed to purge repos/%s" repo)))
      (print! (success "Purged repos/%s" repo))
      t)))

(defun doom--cli-packages-purge-repos (repos)
  (if (not repos)
      (progn (print! (info "No repos to purge"))
             0)
    (length
     (delq nil (mapcar #'doom--cli-packages-purge-repo repos)))))

(defun doom--cli-packages-purge-elpa ()
  (unless (bound-and-true-p package--initialized)
    (package-initialize))
  (let ((packages (cl-loop for (package desc) in package-alist
                           for dir = (package-desc-dir desc)
                           if (file-in-directory-p dir package-user-dir)
                           collect (cons package dir))))
    (if (not package-alist)
        (progn (print! (info "No ELPA packages to purge"))
               0)
      (mapc (doom-rpartial #'delete-directory 'recursive)
            (mapcar #'cdr packages))
      (length packages))))

(defun doom-cli-packages-purge (&optional elpa-p builds-p repos-p regraft-repos-p)
  "Auto-removes orphaned packages and repos.

An orphaned package is a package that isn't a primary package (i.e. doesn't have
a `package!' declaration) or isn't depended on by another primary package.

If BUILDS-P, include straight package builds.
If REPOS-P, include straight repos.
If ELPA-P, include packages installed with package.el (M-x package-install)."
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
         (and (/= 0 (doom--cli-packages-purge-builds builds-to-purge))
              (setq success t)
              (straight-prune-build-cache)))
       (if (not elpa-p)
           (print! (info "Skipping elpa packages"))
         (and (/= 0 (doom--cli-packages-purge-elpa))
              (setq success t)))
       (if (not repos-p)
           (print! (info "Skipping repos"))
         (and (/= 0 (doom--cli-packages-purge-repos repos-to-purge))
              (setq success t)))
       (if (not regraft-repos-p)
           (print! (info "Skipping regrafting"))
         (print! (start "Regrafting %d repos..." (length repos-to-regraft)))
         (and (doom--cli-packages-regraft-repos repos-to-regraft)
              (setq success t)))
       (when success
         (doom--finalize-straight)
         t)))))
