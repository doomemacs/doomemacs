;; -*- no-byte-compile: t; -*-
;;; core/cli/packages.el

(defcli! (update u)
    ((discard-p ["--discard"] "All local changes to packages are discarded"))
  "Updates packages.

This works by fetching all installed package repos and checking the distance
between HEAD and FETCH_HEAD. This can take a while.

This excludes packages whose `package!' declaration contains a non-nil :freeze
or :ignore property."
  (straight-check-all)
  (let ((doom-auto-discard discard-p))
    (doom-cli-reload-core-autoloads)
    (when (doom-cli-packages-update)
      (doom-cli-reload-package-autoloads))
    t))

(defcli! (build b)
    ((rebuild-p ["-r"] "Only rebuild packages that need rebuilding"))
  "Byte-compiles & symlinks installed packages.

This ensures that all needed files are symlinked from their package repo and
their elisp files are byte-compiled. This is especially necessary if you upgrade
Emacs (as byte-code is generally not forward-compatible)."
  (when (doom-cli-packages-build (not rebuild-p))
    (doom-cli-reload-package-autoloads))
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
    (doom-cli-reload-package-autoloads))
  t)

;; (defcli! rollback () ; TODO doom rollback
;;   "<Not implemented yet>"
;;   (user-error "Not implemented yet, sorry!"))


;;
;;; Library

(defun doom--same-commit-p (abbrev-ref ref)
  (and (stringp abbrev-ref)
       (stringp ref)
       (string-match-p (concat "^" (regexp-quote abbrev-ref))
                       ref)))

(defun doom--abbrev-commit (commit &optional full)
  (if full commit (substring commit 0 7)))

(defun doom--commit-log-between (start-ref end-ref)
  (and (straight--call
        "git" "log" "--oneline" "--no-merges"
        "-n" "25" end-ref (concat "^" (regexp-quote start-ref)))
       (straight--process-get-output)))

(defun doom--barf-if-incomplete-packages ()
  (let ((straight-safe-mode t))
    (condition-case _ (straight-check-all)
      (error (user-error "Package state is incomplete. Run 'doom sync' first")))))

(defmacro doom--with-package-recipes (recipes binds &rest body)
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
               ,(doom-enlist binds)
             ,@body))))))

(defvar doom--cli-updated-recipes nil)
(defun doom--cli-recipes-update ()
  "Updates straight and recipe repos."
  (unless doom--cli-updated-recipes
    (straight--make-build-cache-available)
    (print! (start "Updating recipe repos..."))
    (print-group!
     (doom--with-package-recipes
         (delq
          nil (mapcar (doom-rpartial #'gethash straight--repo-cache)
                      (mapcar #'symbol-name straight-recipe-repositories)))
         (recipe package type local-repo)
       (let ((esc (unless doom-debug-mode "\033[1A"))
             (ref (straight-vc-get-commit type local-repo))
             newref output)
         (print! (start "\033[KUpdating recipes for %s...%s") package esc)
         (when (straight-vc-fetch-from-remote recipe)
           (setq output (straight--process-get-output))
           (straight-merge-package package)
           (unless (equal ref (setq newref (straight-vc-get-commit type local-repo)))
             (print! (success "\033[K%s updated (%s -> %s)")
                     package
                     (doom--abbrev-commit ref)
                     (doom--abbrev-commit newref))
             (unless (string-empty-p output)
               (print-group! (print! (info "%s" output)))))))))
    (setq straight--recipe-lookup-cache (make-hash-table :test #'eq)
          doom--cli-updated-recipes t)))


(defun doom-cli-packages-install ()
  "Installs missing packages.

This function will install any primary package (i.e. a package with a `package!'
declaration) or dependency thereof that hasn't already been."
  (doom-initialize-packages)
  (print! (start "Installing packages..."))
  (let ((pinned (doom-package-pinned-list)))
    (print-group!
     (if-let (built
              (doom--with-package-recipes (doom-package-recipe-list)
                  (recipe package type local-repo)
                (unless (file-directory-p (straight--repos-dir local-repo))
                  (doom--cli-recipes-update))
                (condition-case-unless-debug e
                    (let ((straight-use-package-pre-build-functions
                           (cons (lambda (pkg &rest _)
                                   (when-let (commit (cdr (assoc pkg pinned)))
                                     (print! (info "Checked out %s") commit)))
                                 straight-use-package-pre-build-functions)))
                      (straight-use-package (intern package)))
                  (error
                   (signal 'doom-package-error (list package e))))))
         (print! (success "Installed %d packages")
                 (length built))
       (print! (info "No packages need to be installed"))
       nil))))


(defun doom-cli-packages-build (&optional force-p)
  "(Re)build all packages."
  (doom-initialize-packages)
  (print! (start "(Re)building %spackages...") (if force-p "all " ""))
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
         (recipes (doom-package-recipe-list)))
     (unless force-p
       (straight--make-build-cache-available))
     (if-let (built
              (doom--with-package-recipes recipes (package local-repo)
                (unless force-p
                  ;; Ensure packages with outdated files/bytecode are rebuilt
                  (let ((build-dir (straight--build-dir package))
                        (repo-dir  (straight--repos-dir local-repo)))
                    (and (or (file-newer-than-file-p repo-dir build-dir)
                             (file-exists-p (straight--modified-dir (or local-repo package)))
                             ;; Doesn't make sense to compare el and elc files
                             ;; when the former isn't a symlink to their source.
                             (when straight-use-symlinks
                               (cl-loop for file
                                        in (doom-files-in build-dir :match "\\.el$" :full t)
                                        for elc-file = (byte-compile-dest-file file)
                                        if (and (file-exists-p elc-file)
                                                (file-newer-than-file-p file elc-file))
                                        return t)))
                         (puthash package t straight--packages-to-rebuild))))
                (straight-use-package (intern package))))
         (print! (success "Rebuilt %d package(s)") (length built))
       (print! (success "No packages need rebuilding"))
       nil))))


(defun doom-cli-packages-update ()
  "Updates packages."
  (doom-initialize-packages)
  (doom--barf-if-incomplete-packages)
  (let* ((repo-dir (straight--repos-dir))
         (pinned (doom-package-pinned-list))
         (recipes (doom-package-recipe-list))
         (packages-to-rebuild (make-hash-table :test 'equal))
         (repos-to-rebuild (make-hash-table :test 'equal))
         (total (length recipes))
         (esc (unless doom-debug-mode "\033[1A"))
         (i 0)
         errors)
    (when recipes
      (doom--cli-recipes-update))
    (print! (start "Updating packages (this may take a while)..."))
    (doom--with-package-recipes recipes (recipe package type local-repo)
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
           (print! (warn "(%d/%d) Skipping %s because it is local") i total package)
           (cl-return))
         (when (eq type 'git)
           (unless (file-exists-p ".git")
             (error "%S is not a valid repository" package)))
         (condition-case-unless-debug e
             (let ((ref (straight-vc-get-commit type local-repo))
                   (target-ref
                    (cdr (or (assoc local-repo pinned)
                             (assoc package pinned))))
                   output)
               (or (cond
                    ((not (stringp target-ref))
                     (print! (start "\033[K(%d/%d) Fetching %s...%s") i total package esc)
                     (when (straight-vc-fetch-from-remote recipe)
                       (setq output (straight--process-get-output))
                       (straight-merge-package package)
                       (setq target-ref (straight-vc-get-commit type local-repo))
                       (or (not (doom--same-commit-p target-ref ref))
                           (cl-return))))

                    ((doom--same-commit-p target-ref ref)
                     (print! (info "\033[K(%d/%d) %s is up-to-date...%s") i total package esc)
                     (cl-return))

                    ((if (straight-vc-commit-present-p recipe target-ref)
                         (print! (start "\033[K(%d/%d) Checking out %s (%s)...%s")
                                 i total package (doom--abbrev-commit target-ref) esc)
                       (print! (start "\033[K(%d/%d) Fetching %s...%s") i total package esc)
                       (and (straight-vc-fetch-from-remote recipe)
                            (straight-vc-commit-present-p recipe target-ref)))
                     (straight-vc-check-out-commit recipe target-ref)
                     (or (not (eq type 'git))
                         (setq output (doom--commit-log-between ref target-ref)))
                     (doom--same-commit-p target-ref (straight-vc-get-commit type local-repo)))

                    ((print! (start "\033[K(%d/%d) Re-cloning %s...") i total local-repo esc)
                     (let ((repo (straight--repos-dir local-repo))
                           (straight-vc-git-default-clone-depth 'full))
                       (delete-directory repo 'recursive)
                       (print-group!
                        (straight-use-package (intern package) nil 'no-build))
                       (prog1 (file-directory-p repo)
                         (or (not (eq type 'git))
                             (setq output (doom--commit-log-between ref target-ref)))))))
                   (progn
                     (print! (warn "\033[K(%d/%d) Failed to fetch %s")
                             i total local-repo)
                     (unless (string-empty-p output)
                       (print-group! (print! (info "%s" output))))
                     (cl-return)))
               (puthash local-repo t repos-to-rebuild)
               (puthash package t packages-to-rebuild)
               (unless (string-empty-p output)
                 (print! (start "\033[K(%d/%d) Updating %s...") i total local-repo)
                 (print-group! (print! "%s" (indent 2 output))))
               (print! (success "\033[K(%d/%d) %s updated (%s -> %s)")
                       i total local-repo
                       (doom--abbrev-commit ref)
                       (doom--abbrev-commit target-ref)))
           (user-error
            (signal 'user-error (error-message-string e)))
           (error
            (signal 'doom-package-error (list package e)))))))
    (print-group!
     (princ "\033[K")
     (if (hash-table-empty-p packages-to-rebuild)
         (ignore (print! (success "All %d packages are up-to-date") total))
       (straight--transaction-finalize)
       (let ((default-directory (straight--build-dir)))
         (mapc (doom-rpartial #'delete-directory 'recursive)
               (hash-table-keys packages-to-rebuild)))
       (print! (success "Updated %d package(s)")
               (hash-table-count packages-to-rebuild))
       (doom-cli-packages-build)
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
      (prog1 0
        (print! (info "No builds to purge")))
    (print! (start "Purging straight builds..." (length builds)))
    (print-group!
     (length
      (delq nil (mapcar #'doom--cli-packages-purge-build builds))))))

(cl-defun doom--cli-packages-regraft-repo (repo)
  (let ((default-directory (straight--repos-dir repo)))
    (unless (file-directory-p ".git")
      (print! (warn "\033[Krepos/%s is not a git repo, skipping" repo))
      (cl-return))
    (unless (file-in-directory-p default-directory straight-base-dir)
      (print! (warn "\033[KSkipping repos/%s because it is local" repo))
      (cl-return))
    (let ((before-size (doom-directory-size default-directory)))
      (straight--call "git" "reset" "--hard")
      (straight--call "git" "clean" "-ffd")
      (if (not (car (straight--call "git" "replace" "--graft" "HEAD")))
          (print! (info "\033[Krepos/%s is already compact\033[1A" repo))
        (straight--call "git" "reflog" "expire" "--expire=all" "--all")
        (straight--call "git" "gc" "--prune=now")
        (print! (success "\033[KRegrafted repos/%s (from %0.1fKB to %0.1fKB)")
                repo before-size (doom-directory-size default-directory))
        (print-group! (print! "%s" (straight--process-get-output))))
      t)))

(defun doom--cli-packages-regraft-repos (repos)
  (if (not repos)
      (prog1 0
        (print! (info "No repos to regraft")))
    (print! (start "Regrafting %d repos..." (length repos)))
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
    (delete-file (straight--modified-file repo))
    (if (file-directory-p repo-dir)
        (ignore (print! (error "Failed to purge repos/%s" repo)))
      (print! (success "Purged repos/%s" repo))
      t)))

(defun doom--cli-packages-purge-repos (repos)
  (if (not repos)
      (prog1 0
        (print! (info "No repos to purge")))
    (print! (start "Purging straight repositories..."))
    (print-group!
     (length
      (delq nil (mapcar #'doom--cli-packages-purge-repo repos))))))

(defun doom--cli-packages-purge-elpa ()
  (require 'core-packages)
  (let ((dirs (doom-files-in package-user-dir :type t :depth 0)))
    (if (not dirs)
        (prog1 0
          (print! (info "No ELPA packages to purge")))
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

(defun doom-cli-packages-purge (&optional elpa-p builds-p repos-p regraft-repos-p)
  "Auto-removes orphaned packages and repos.

An orphaned package is a package that isn't a primary package (i.e. doesn't have
a `package!' declaration) or isn't depended on by another primary package.

If BUILDS-P, include straight package builds.
If REPOS-P, include straight repos.
If ELPA-P, include packages installed with package.el (M-x package-install)."
  (doom-initialize-packages)
  (doom--barf-if-incomplete-packages)
  (print! (start "Purging orphaned packages (for the emperor)..."))
  (cl-destructuring-bind (&optional builds-to-purge repos-to-purge repos-to-regraft)
      (let ((rdirs (straight--directory-files (straight--repos-dir) nil nil 'sort))
            (bdirs (straight--directory-files (straight--build-dir) nil nil 'sort)))
        (list (seq-remove (doom-rpartial #'gethash straight--profile-cache)
                          bdirs)
              (seq-remove (doom-rpartial #'straight--checkhash straight--repo-cache)
                          rdirs)
              (seq-filter (doom-rpartial #'straight--checkhash straight--repo-cache)
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
         (and (doom--cli-packages-regraft-repos repos-to-regraft)
              (setq success t)))
       success))))
