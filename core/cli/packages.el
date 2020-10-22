;; -*- no-byte-compile: t; -*-
;;; core/cli/packages.el

(defcli! (update u) (&rest _)
  "This command was removed."
  :hidden t
  (print! (error "This command has been removed.\n"))
  (print-group!
   (print! "To update Doom run 'doom upgrade'. To only update packages run 'doom sync -u'."))
  nil)

(defcli! (build b)
    ((rebuild-p ["-r"] "Only rebuild packages that need rebuilding"))
  "Byte-compiles & symlinks installed packages.

This ensures that all needed files are symlinked from their package repo and
their elisp files are byte-compiled. This is especially necessary if you upgrade
Emacs (as byte-code is generally not forward-compatible)."
  (when (doom-cli-packages-build (not rebuild-p))
    (doom-autoloads-reload))
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
    (doom-autoloads-reload))
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
  (when-let*
      ((status (straight--call
                "git" "log" "--oneline" "--no-merges"
                "-n" "26" end-ref (concat "^" (regexp-quote start-ref))))
       (output (string-trim-right (straight--process-get-output)))
       (lines (split-string output "\n")))
    (if (> (length lines) 25)
        (concat (string-join (butlast lines 1) "\n") "\n[...]")
      output)))

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
       (let ((esc (unless doom-debug-p "\033[1A"))
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

(defvar doom--eln-output-expected nil)

(defvar doom--eln-output-path (car (bound-and-true-p comp-eln-load-path)))

(defun doom--eln-file-name (file)
  "Return the short .eln file name corresponding to `file'."
  (concat comp-native-version-dir "/"
          (file-name-nondirectory
           (comp-el-to-eln-filename file))))

(defun doom--eln-output-file (eln-name)
  "Return the expected .eln file corresponding to `eln-name'."
  (concat doom--eln-output-path eln-name))

(defun doom--eln-error-file (eln-name)
  "Return the expected .error file corresponding to `eln-name'."
  (concat doom--eln-output-path eln-name ".error"))

(defun doom--find-eln-file (eln-name)
  "Find `eln-name' on the `comp-eln-load-path'."
  (cl-some (lambda (eln-path)
             (let ((file (concat eln-path eln-name)))
               (when (file-exists-p file)
                 file)))
           comp-eln-load-path))

(defun doom--elc-file-outdated-p (file)
  "Check whether the corresponding .elc for `file' is outdated."
  (let ((elc-file (byte-compile-dest-file file)))
    ;; NOTE Ignore missing elc files, they could be missing due to
    ;; `no-byte-compile'. Rebuilding unnecessarily is expensive.
    (when (and (file-exists-p elc-file)
               (file-newer-than-file-p file elc-file))
      (doom-log "%s is newer than %s" file elc-file)
      t)))

(defun doom--eln-file-outdated-p (file)
  "Check whether the corresponding .eln for `file' is outdated."
  (let* ((eln-name (doom--eln-file-name file))
         (eln-file (doom--find-eln-file eln-name))
         (error-file (doom--eln-error-file eln-name)))
    (cond (eln-file
           (when (file-newer-than-file-p file eln-file)
             (doom-log "%s is newer than %s" file eln-file)
             t))
          ((file-exists-p error-file)
           (when (file-newer-than-file-p file error-file)
             (doom-log "%s is newer than %s" file error-file)
             t))
          (t
           (doom-log "%s doesn't exist" eln-name)
           t))))

(defun doom--native-compile-done-h (file)
  "Callback fired when an item has finished async compilation."
  (when file
    (let* ((eln-name (doom--eln-file-name file))
           (eln-file (doom--eln-output-file eln-name))
           (error-file (doom--eln-error-file eln-name)))
      (if (file-exists-p eln-file)
          (doom-log "Compiled %s" eln-file)
        (make-directory (file-name-directory error-file) 'parents)
        (write-region "" nil error-file)
        (doom-log "Wrote %s" error-file)))))

(defun doom--native-compile-jobs ()
  "How many async native compilation jobs are queued or in-progress."
  (if (and (boundp 'comp-files-queue)
           (fboundp 'comp-async-runnings))
      (+ (length comp-files-queue)
         (comp-async-runnings))
    0))

(defun doom--wait-for-compile-jobs ()
  "Wait for all pending async native compilation jobs."
  (cl-loop for pending = (doom--native-compile-jobs)
           with previous = 0
           while (not (zerop pending))
           if (/= previous pending) do
           (print! (info "\033[KWaiting for %d async jobs...\033[1A" pending))
           (setq previous pending)
           else do
           (let ((inhibit-message t))
             (sleep-for 0.1))))

(defun doom--write-missing-eln-errors ()
  "Write .error files for any expected .eln files that are missing."
  (cl-loop for file in doom--eln-output-expected
           for eln-name = (doom--eln-file-name file)
           for eln-file = (doom--eln-output-file eln-name)
           for error-file = (doom--eln-error-file eln-name)
           unless (or (file-exists-p eln-file)
                      (file-newer-than-file-p error-file file))
           do (make-directory (file-name-directory error-file) 'parents)
              (write-region "" nil error-file)
              (doom-log "Wrote %s" error-file))
  (setq doom--eln-output-expected nil))

(defun doom--compile-site-packages ()
  "Queue async compilation for all non-doom Elisp files."
  (when (fboundp 'native-compile-async)
    (cl-loop with paths = (cl-loop for path in load-path
                                   unless (string-prefix-p doom-local-dir path)
                                   collect path)
             for file in (doom-files-in paths :match "\\.el\\(?:\\.gz\\)?$")
             if (and (file-exists-p (byte-compile-dest-file file))
                     (not (doom--find-eln-file (doom--eln-file-name file)))) do
             (doom-log "Compiling %s" file)
             (native-compile-async file nil 'late))))


(defun doom-cli-packages-install ()
  "Installs missing packages.

This function will install any primary package (i.e. a package with a `package!'
declaration) or dependency thereof that hasn't already been."
  (doom-initialize-packages)
  (print! (start "Installing packages..."))
  (let ((pinned (doom-package-pinned-list)))
    (print-group!
     (add-hook 'comp-async-cu-done-hook #'doom--native-compile-done-h)
     (if-let (built
              (doom--with-package-recipes (doom-package-recipe-list)
                  (recipe package type local-repo)
                (unless (file-directory-p (straight--repos-dir local-repo))
                  (doom--cli-recipes-update))
                (condition-case-unless-debug e
                    (let ((straight-use-package-pre-build-functions
                           (cons (lambda (pkg &rest _)
                                   (when-let (commit (cdr (assoc pkg pinned)))
                                     (print! (info "Checked out %s: %s") pkg commit)))
                                 straight-use-package-pre-build-functions)))
                      (straight-use-package (intern package))
                      ;; HACK Line encoding issues can plague repos with dirty
                      ;;      worktree prompts when updating packages or "Local
                      ;;      variables entry is missing the suffix" errors when
                      ;;      installing them (see hlissner/doom-emacs#2637), so
                      ;;      have git handle conversion by force.
                      (when (and IS-WINDOWS (stringp local-repo))
                        (let ((default-directory (straight--repos-dir local-repo)))
                          (when (file-in-directory-p default-directory straight-base-dir)
                            (straight--call "git" "config" "core.autocrlf" "true")))))
                  (error
                   (signal 'doom-package-error (list package e))))))
         (progn
           (doom--compile-site-packages)
           (doom--wait-for-compile-jobs)
           (doom--write-missing-eln-errors)
           (print! (success "Installed %d packages") (length built)))
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
     (add-hook 'comp-async-cu-done-hook #'doom--native-compile-done-h)
     (unless force-p
       (straight--make-build-cache-available))
     (if-let (built
              (doom--with-package-recipes recipes (package local-repo recipe)
                (unless force-p
                  ;; Ensure packages with outdated files/bytecode are rebuilt
                  (let ((build-dir (straight--build-dir package))
                        (repo-dir  (straight--repos-dir local-repo)))
                    (and (not (plist-get recipe :no-build))
                         (or (file-newer-than-file-p repo-dir build-dir)
                             (file-exists-p (straight--modified-dir (or local-repo package)))
                             (cl-loop with want-byte   = (straight--byte-compile-package-p recipe)
                                      with want-native = (if (require 'comp nil t) (straight--native-compile-package-p recipe))
                                      with outdated = nil
                                      for file in (doom-files-in build-dir :match "\\.el$" :full t)
                                      if (or (if want-byte   (doom--elc-file-outdated-p file))
                                             (if want-native (doom--eln-file-outdated-p file)))
                                      do (setq outdated t)
                                         (when want-native
                                           (push file doom--eln-output-expected))
                                      finally return outdated))
                         (puthash package t straight--packages-to-rebuild))))
                (straight-use-package (intern package))))
         (progn
           (doom--compile-site-packages)
           (doom--wait-for-compile-jobs)
           (doom--write-missing-eln-errors)
           (print! (success "\033[KRebuilt %d package(s)") (length built)))
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
         (esc (unless doom-debug-p "\033[1A"))
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
               (print! (success "\033[K(%d/%d) %s updated (%s -> %s)")
                       i total local-repo
                       (doom--abbrev-commit ref)
                       (doom--abbrev-commit target-ref))
               (unless (string-empty-p output)
                 (print-group! (print! "%s" (indent 2 output)))))
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
               (ignore (print! (info "Skipping builds")))
             (and (/= 0 (doom--cli-packages-purge-builds builds-to-purge))
                  (straight-prune-build-cache)))
           (if (not elpa-p)
               (ignore (print! (info "Skipping elpa packages")))
             (/= 0 (doom--cli-packages-purge-elpa)))
           (if (not repos-p)
               (ignore (print! (info "Skipping repos")))
             (/= 0 (doom--cli-packages-purge-repos repos-to-purge)))
           (if (not regraft-repos-p)
               (ignore (print! (info "Skipping regrafting")))
             (doom--cli-packages-regraft-repos repos-to-regraft)))))))
