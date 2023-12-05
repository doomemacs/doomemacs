;;; lisp/cli/packages.el --- package management commands -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'comp nil t)

;;
;;; Variables

;; None yet!


;;
;;; Commands

(defcli! (:before (build b purge p)) (&context context)
  (require 'comp nil t)
  (doom-initialize-core-packages))

;; DEPRECATED Replace with "doom sync --rebuild"
(defcli! ((build b))
    ((rebuild-p ("-r") "Only rebuild packages that need rebuilding")
     (jobs      ("-j" "--jobs" num) "How many CPUs to use for native compilation"))
  "Byte-compiles & symlinks installed packages.

This ensures that all needed files are symlinked from their package repo and
their elisp files are byte-compiled. This is especially necessary if you upgrade
Emacs (as byte-code is generally not forward-compatible)."
  :benchmark t
  (when jobs
    (setq native-comp-async-jobs-number (truncate jobs)))
  (when (doom-packages-build (not rebuild-p))
    (doom-profile-generate))
  t)

;; TODO Rename to "doom gc" and move to its own file
(defcli! ((purge p))
    ((nobuilds-p  ("-b" "--no-builds")  "Don't purge unneeded (built) packages")
     (noelpa-p    ("-p" "--no-elpa")    "Don't purge ELPA packages")
     (norepos-p   ("-r" "--no-repos")   "Don't purge unused straight repos")
     (noeln-p     ("-e" "--no-eln")     "Don't purge old ELN bytecode")
     (noregraft-p ("-g" "--no-regraft") "Regraft git repos (ie. compact them)"))
  "Deletes orphaned packages & repos, and compacts them.

Purges all installed ELPA packages (as they are considered temporary). Purges
all orphaned package repos and builds. If -g/--regraft is supplied, the git
repos among them will be regrafted and compacted to ensure they are as small as
possible.

It is a good idea to occasionally run this doom purge -g to ensure your package
list remains lean."
  :benchmark t
  (straight-check-all)
  (when (doom-packages-purge
         (not noelpa-p)
         (not norepos-p)
         (not nobuilds-p)
         (not noregraft-p)
         (not noeln-p))
    (doom-profile-generate))
  t)

(defcli-stub! rollback)  ; TODO Implement me post-3.0


;;
;;; Library

;; FIXME Enforce naming conventions for all functions below

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
      (let ((esc (unless init-file-debug "\033[1A"))
            (ref (straight-vc-get-commit type local-repo))
            newref output)
        (print! (start "\033[KUpdating recipes for %s...%s") package esc)
        (doom-packages--straight-with (straight-vc-fetch-from-remote recipe)
          (when .it
            (setq output .output)
            (straight-merge-package package)
            (unless (equal ref (setq newref (straight-vc-get-commit type local-repo)))
              (print! (success "\033[K%s updated (%s -> %s)")
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
           for pending = (+ (length comp-files-queue) (comp-async-runnings))
           while (not (zerop pending))
           if (/= previous pending) do
           (print! (start "\033[KNatively compiling %d files...\033[1A" pending))
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
  (when (featurep 'native-compile)
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
    (setq doom-packages--eln-output-expected nil)))

(defun doom-packages--compile-site-files ()
  "Queue async compilation for all non-doom Elisp files."
  (when (featurep 'native-compile)
    (cl-loop with paths = (cl-loop for path in load-path
                                   unless (file-in-directory-p path doom-local-dir)
                                   collect path)
             for file in (doom-files-in paths :match "\\.el\\(?:\\.gz\\)?$")
             if (and (file-exists-p (byte-compile-dest-file file))
                     (not (doom-packages--find-eln-file (doom-packages--eln-file-name file)))
                     (not (cl-some (fn! (string-match-p % file))
                                   native-comp-deferred-compilation-deny-list))) do
             (doom-log "Compiling %s" file)
             (native-compile-async file))))

(defun doom-packages-install ()
  "Installs missing packages.

This function will install any primary package (i.e. a package with a `package!'
declaration) or dependency thereof that hasn't already been."
  (doom-initialize-packages)
  (print! (start "Installing packages..."))
  (let ((pinned (doom-package-pinned-list)))
    (print-group!
     (add-hook 'native-comp-async-cu-done-functions #'doom-packages--native-compile-done-h)
     (if-let (built
              (doom-packages--with-recipes (doom-package-recipe-list)
                  (recipe package type local-repo)
                (unless (file-directory-p (straight--repos-dir local-repo))
                  (doom-packages--cli-recipes-update))
                (condition-case-unless-debug e
                    (let ((straight-use-package-pre-build-functions
                           (cons (lambda (pkg &rest _)
                                   (when-let (commit (cdr (assoc pkg pinned)))
                                     (print! (item "Checked out %s: %s") pkg commit)))
                                 straight-use-package-pre-build-functions)))
                      (straight-use-package (intern package))
                      ;; HACK Line encoding issues can plague repos with dirty
                      ;;      worktree prompts when updating packages or "Local
                      ;;      variables entry is missing the suffix" errors when
                      ;;      installing them (see hlissner/doom-emacs#2637), so
                      ;;      have git handle conversion by force.
                      (when (and doom--system-windows-p (stringp local-repo))
                        (let ((default-directory (straight--repos-dir local-repo)))
                          (when (file-in-directory-p default-directory straight-base-dir)
                            (straight--process-run "git" "config" "core.autocrlf" "true")))))
                  (error
                   (signal 'doom-package-error (list package e))))))
         (progn
           (when (featurep 'native-compile)
             (doom-packages--compile-site-files)
             (doom-packages--wait-for-native-compile-jobs)
             (doom-packages--write-missing-eln-errors))
           (print! (success "\033[KInstalled %d packages") (length built)))
       (print! (item "No packages need to be installed"))
       nil))))


(defun doom-packages-build (&optional force-p)
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
     (add-hook 'native-comp-async-cu-done-functions #'doom-packages--native-compile-done-h)
     (unless force-p
       (straight--make-build-cache-available))
     (if-let (built
              (doom-packages--with-recipes recipes (package local-repo recipe)
                (unless force-p
                  ;; Ensure packages with outdated files/bytecode are rebuilt
                  (let* ((build-dir (straight--build-dir package))
                         (repo-dir  (straight--repos-dir local-repo))
                         (build (if (plist-member recipe :build)
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
                    (unless (featurep 'native-compile)
                      (setq want-native-compile nil))
                    (and (or want-byte-compile want-native-compile)
                         (or (file-newer-than-file-p repo-dir build-dir)
                             (file-exists-p (straight--modified-dir (or local-repo package)))
                             (cl-loop with outdated = nil
                                      for file in (doom-files-in build-dir :match "\\.el$" :full t)
                                      if (or (if want-byte-compile   (doom-packages--elc-file-outdated-p file))
                                             (if want-native-compile (doom-packages--eln-file-outdated-p file)))
                                      do (setq outdated t)
                                         (when want-native-compile
                                           (push file doom-packages--eln-output-expected))
                                      finally return outdated))
                         (puthash package t straight--packages-to-rebuild))))
                (straight-use-package (intern package))))
         (progn
           (when (featurep 'native-compile)
             (doom-packages--compile-site-files)
             (doom-packages--wait-for-native-compile-jobs)
             (doom-packages--write-missing-eln-errors))
           ;; HACK Every time you save a file in a package that straight tracks,
           ;;      it is recorded in ~/.emacs.d/.local/straight/modified/.
           ;;      Typically, straight will clean these up after rebuilding, but
           ;;      Doom's use-case circumnavigates that, leaving these files
           ;;      there and causing a rebuild of those packages each time `doom
           ;;      sync' or similar is run, so we clean it up ourselves:
           (delete-directory (straight--modified-dir) 'recursive)
           (print! (success "\033[KRebuilt %d package(s)") (length built)))
       (print! (item "No packages need rebuilding"))
       nil))))



(defun doom-packages-update ()
  "Updates packages."
  (doom-initialize-packages)
  (doom-packages--barf-if-incomplete)
  (doom-packages--cli-recipes-update)
  (let* ((repo-dir (straight--repos-dir))
         (pinned (doom-package-pinned-list))
         (recipes (doom-package-recipe-list))
         (packages-to-rebuild (make-hash-table :test 'equal))
         (repos-to-rebuild (make-hash-table :test 'equal))
         (total (length recipes))
         (esc (unless init-file-debug "\033[1A"))
         (i 0)
         errors)
    (print! (start "Updating packages (this may take a while)..."))
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
                   commits
                   output)
               (or (cond
                    ((not (stringp target-ref))
                     (print! (start "\033[K(%d/%d) Fetching %s...%s") i total package esc)
                     (doom-packages--straight-with (straight-vc-fetch-from-remote recipe)
                       (when .it
                         (straight-merge-package package)
                         ;; (condition-case e
                         ;;     (straight-merge-package package)
                         ;;   (wrong-type-argument
                         ;;    (if (not (equal (cdr e) '(arrayp nil)))
                         ;;        (signal (car e) (cdr e))
                         ;;      (delete-directory (straight--build-dir local-repo) t)
                         ;;      (straight-use-package (intern package)))))
                         (setq target-ref (straight-vc-get-commit type local-repo))
                         (setq output (doom-packages--commit-log-between ref target-ref)
                               commits (length (split-string output "\n" t)))
                         (or (not (doom-packages--same-commit-p target-ref ref))
                             (cl-return)))))

                    ((doom-packages--same-commit-p target-ref ref)
                     (print! (item "\033[K(%d/%d) %s is up-to-date...%s") i total package esc)
                     (cl-return))

                    ((if (straight-vc-commit-present-p recipe target-ref)
                         (print! (start "\033[K(%d/%d) Checking out %s (%s)...%s")
                                 i total package (doom-packages--abbrev-commit target-ref) esc)
                       (print! (start "\033[K(%d/%d) Fetching %s...%s") i total package esc)
                       (and (straight-vc-fetch-from-remote recipe)
                            (straight-vc-commit-present-p recipe target-ref)))
                     (straight-vc-check-out-commit recipe target-ref)
                     (or (not (eq type 'git))
                         (setq output (doom-packages--commit-log-between ref target-ref)
                               commits (length (split-string output "\n" t))))
                     (doom-packages--same-commit-p target-ref (straight-vc-get-commit type local-repo)))

                    ((print! (start "\033[K(%d/%d) Re-cloning %s...") i total local-repo esc)
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
                     (print! (warn "\033[K(%d/%d) Failed to fetch %s")
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
                 (print! (success "\033[K(%d/%d) %s: %s -> %s%s%s")
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
     (princ "\033[K")
     (if (hash-table-empty-p packages-to-rebuild)
         (ignore (print! (success "All %d packages are up-to-date") total))
       (straight--transaction-finalize)
       (let ((default-directory (straight--build-dir)))
         (mapc (doom-rpartial #'delete-directory 'recursive)
               (hash-table-keys packages-to-rebuild)))
       (print! (success "Updated %d package(s)")
               (hash-table-count packages-to-rebuild))
       (doom-packages-build)
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
      (print! (warn "\033[Krepos/%s is not a git repo, skipping" repo))
      (cl-return))
    (unless (file-in-directory-p default-directory straight-base-dir)
      (print! (warn "\033[KSkipping repos/%s because it is local" repo))
      (cl-return))
    (let ((before-size (doom-directory-size default-directory)))
      (doom-call-process "git" "reset" "--hard")
      (doom-call-process "git" "clean" "-ffd")
      (if (not (zerop (car (doom-call-process "git" "replace" "--graft" "HEAD"))))
          (print! (item "\033[Krepos/%s is already compact\033[1A" repo))
        (doom-call-process "git" "reflog" "expire" "--expire=all" "--all")
        (doom-call-process "git" "gc" "--prune=now")
        (let ((after-size (doom-directory-size default-directory)))
          (if (equal after-size before-size)
              (print! (success "\033[Krepos/%s cannot be compacted further" repo))
            (print! (success "\033[KRegrafted repos/%s (from %0.1fKB to %0.1fKB)")
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
         (princ "\033[K")
         (let ((after-size (doom-directory-size (straight--repos-dir))))
           (print! (success "Finished regrafting. Size before: %0.1fKB and after: %0.1fKB (%0.1fKB)")
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
  (require 'doom-packages)
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
  (if-let (dirs
           (cl-delete (expand-file-name comp-native-version-dir doom-packages--eln-output-path)
                      (directory-files doom-packages--eln-output-path t "^[^.]" t)
                      :test #'file-equal-p))
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


;;
;;; Hacks

;; Straight was designed primarily for interactive use, in an interactive Emacs
;; session, but Doom does its package management in the terminal. Some things
;; must be modified get straight to behave and improve its UX for our users.

(defvar doom-cli--straight-auto-options
  '(("has diverged from"
     . "^Reset [^ ]+ to branch")
    ("but recipe specifies a URL of"
     . "Delete remote \"[^\"]+\", re-create it with correct URL")
    ("has a merge conflict:"
     . "^Abort merge$")
    ("has a dirty worktree:"
     . "^Discard changes$")
    ("^In repository \"[^\"]+\", [^ ]+ (on branch \"main\") is ahead of default branch \"master\""
     . "^Checkout branch \"master\"")
    ("^In repository \"[^\"]+\", [^ ]+ (on branch \"[^\"]+\") is ahead of default branch \"[^\"]+\""
     . "^Checkout branch \"")
    ("^In repository "
     . "^Reset branch \\|^Delete remote [^,]+, re-create it with correct URL\\|^Checkout \"main\""))
  "A list of regexps, mapped to regexps.

Their CAR is tested against the prompt, and CDR is tested against the presented
option, and is used by `straight-vc-git--popup-raw' to select which option to
recommend.

It may not be obvious to users what they should do for some straight prompts,
so Doom will recommend the one that reverts a package back to its (or target)
original state.")

;; HACK Remove dired & magit options from prompt, since they're inaccessible in
;;      noninteractive sessions.
(advice-add #'straight-vc-git--popup-raw :override #'straight--popup-raw)

;; HACK Replace GUI popup prompts (which hang indefinitely in tty Emacs) with
;;      simple prompts.
(defadvice! doom-cli--straight-fallback-to-y-or-n-prompt-a (fn &optional prompt noprompt?)
  :around #'straight-are-you-sure
  (or noprompt?
      (if noninteractive
          (y-or-n-p (format! "%s" (or prompt "")))
        (funcall fn prompt))))

(defun doom-cli--straight-recommended-option-p (prompt option)
  (cl-loop for (prompt-re . opt-re) in doom-cli--straight-auto-options
           if (string-match-p prompt-re prompt)
           return (string-match-p opt-re option)))

(defadvice! doom-cli--straight-no-compute-prefixes-a (fn &rest args)
  :around #'straight--build-autoloads
  (let (autoload-compute-prefixes)
    (apply fn args)))

(defadvice! doom-cli--straight-fallback-to-tty-prompt-a (fn prompt actions)
  "Modifies straight to prompt on the terminal when in noninteractive sessions."
  :around #'straight--popup-raw
  (if (bound-and-true-p async-in-child-emacs)
      (error "Straight prompt: %s" prompt)
    (let ((doom-cli--straight-auto-options doom-cli--straight-auto-options))
      ;; We can't intercept C-g, so no point displaying any options for this key
      ;; when C-c is the proper way to abort batch Emacs.
      (delq! "C-g" actions 'assoc)
      ;; HACK: These are associated with opening dired or magit, which isn't
      ;;   possible in tty Emacs, so...
      (delq! "e" actions 'assoc)
      (delq! "g" actions 'assoc)
      (if (doom-cli-context-suppress-prompts-p doom-cli--context)
          (cl-loop for (_key desc func) in actions
                   when desc
                   when (doom-cli--straight-recommended-option-p prompt desc)
                   return (funcall func))
        (print! (start "%s") (red prompt))
        (print-group!
         (terpri)
         (let (recommended options)
           (print-group!
            (print! " 1) Abort")
            (cl-loop for (_key desc func) in actions
                     when desc
                     do (push func options)
                     and do
                     (print! "%2s) %s" (1+ (length options))
                             (if (doom-cli--straight-recommended-option-p prompt desc)
                                 (progn
                                   (setq doom-cli--straight-auto-options nil
                                         recommended (length options))
                                   (green (concat desc " (Choose this if unsure)")))
                               desc))))
           (terpri)
           (let* ((options
                   (cons (lambda ()
                           (let ((doom-output-indent 0))
                             (terpri)
                             (print! (warn "Aborted")))
                           (doom-cli--exit 1))
                         (nreverse options)))
                  (prompt
                   (format! "How to proceed? (%s%s) "
                            (mapconcat #'number-to-string
                                       (number-sequence 1 (length options))
                                       ", ")
                            (if (not recommended) ""
                              (format "; don't know? Pick %d" (1+ recommended)))))
                  answer fn)
             (while (null (nth (setq answer (1- (read-number prompt))) options))
               (print! (warn "%s is not a valid answer, try again.") answer))
             (funcall (nth answer options)))))))))

(setq straight-arrow " > ")
(defadvice! doom-cli--straight-respect-print-indent-a (string &rest objects)
  "Same as `message' (which see for STRING and OBJECTS) normally.
However, in batch mode, print to stdout instead of stderr."
  :override #'straight--output
  (let ((msg (apply #'format string objects)))
    (save-match-data
      (when (string-match (format "^%s\\(.+\\)$" (regexp-quote straight-arrow)) msg)
        (setq msg (match-string 1 msg))))
    (and (string-match-p "^\\(Cloning\\|\\(Reb\\|B\\)uilding\\) " msg)
         (not (string-suffix-p "...done" msg))
         (doom-print (concat "> " msg) :format t))))

(defadvice! doom-cli--straight-ignore-gitconfig-a (fn &rest args)
  "Prevent user and system git configuration from interfering with git calls."
  :around #'straight--process-call
  (letenv! (("GIT_CONFIG" nil)
            ("GIT_CONFIG_NOSYSTEM" "1")
            ("GIT_CONFIG_GLOBAL" (or (getenv "DOOMGITCONFIG")
                                     "/dev/null")))
    (apply fn args)))

(provide 'doom-cli-packages)
;;; packages.el ends here
