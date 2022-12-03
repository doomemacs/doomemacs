;;; lisp/cli/upgrade.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(load! "packages")
(load! "compile")


;;
;;; Variables

(defvar doom-upgrade-url "https://github.com/doomemacs/doomemacs"
  "The git repo url for Doom Emacs.")

(defvar doom-upgrade-remote "_upgrade"
  "The name to use as our staging remote.")


;;
;;; Commands

(defcli! ((upgrade up))
    ((packages?  ("-p" "--packages") "Only upgrade packages, not Doom")
     (jobs       ("-j" "--jobs" num) "How many CPUs to use for native compilation")
     &context context)
  "Updates Doom and packages.

This requires that ~/.emacs.d is a git repo, and is the equivalent of the
following shell commands:

    cd ~/.emacs.d
    git pull --rebase
    doom clean
    doom sync -u"
  (let* ((force? (doom-cli-context-suppress-prompts-p context))
         (sync-cmd (append '("sync" "-u") (if jobs `("-j" ,num)))))
    (cond
     (packages?
      ;; HACK It's messy to use straight to upgrade straight, due to the
      ;;   potential for backwards incompatibility, so we staticly check if
      ;;   Doom's `package!' declaration for straight has changed. If it has,
      ;;   delete straight so 'doom sync' will install the new version for us.
      ;;
      ;;   Clumsy, but a better solution is in the works.
      (let ((recipe (doom-cli-context-get context 'straight-recipe)))
        (when (and recipe (not (equal recipe (doom-upgrade--get-straight-recipe))))
          (print! (item "Preparing straight for an update"))
          (delete-directory (doom-path straight-base-dir "straight/repos/straight.el")
                            'recursive)))
      (call! sync-cmd)
      (print! (success "Finished upgrading Doom Emacs")))

     ((doom-cli-upgrade context force? force?)
      ;; Reload Doom's CLI & libraries, in case there were any upstream changes.
      ;; Major changes will still break, however
      (print! (item "Reloading Doom Emacs"))
      (exit! "doom" "upgrade" "-p"
             (if force? "--force")
             (if jobs (format "--jobs=%d" jobs))))

     ((print! "Doom is up-to-date!")
      (call! sync-cmd)))))


;;
;;; Helpers

(defun doom-cli-upgrade (context &optional auto-accept-p force-p)
  "Upgrade Doom to the latest version non-destructively."
  (let ((default-directory doom-emacs-dir)
        process-file-side-effects)
    (print! (start "Preparing to upgrade Doom Emacs and its packages..."))

    (let* (;; git name-rev may return BRANCH~X for detached HEADs and fully
           ;; qualified refs in some other cases, so an effort to strip out all
           ;; but the branch name is necessary. git symbolic-ref (or
           ;; `vc-git--symbolic-ref') won't work; it can't deal with submodules.
           (branch (replace-regexp-in-string
                    "^\\(?:[^/]+/[^/]+/\\)?\\(.+\\)\\(?:~[0-9]+\\)?$" "\\1"
                    (cdr (sh! "git" "name-rev" "--name-only" "HEAD"))))
           (target-remote (format "%s_%s" doom-upgrade-remote branch)))
      (unless branch
        (error (if (file-exists-p! ".git" doom-emacs-dir)
                   "Couldn't find Doom's .git directory. Was Doom cloned properly?"
                 "Couldn't detect what branch you're on. Is Doom detached?")))

      ;; We assume that a dirty .emacs.d is intentional and abort
      (when-let (dirty (doom-upgrade--working-tree-dirty-p default-directory))
        (if (not force-p)
            (user-error "%s\n\n%s\n\n %s"
                        (format "Refusing to upgrade because %S has been modified."
                                (abbreviate-file-name doom-emacs-dir))
                        "Either stash/undo your changes or run 'doom upgrade --force' to discard local changes."
                        (string-join dirty "\n"))
          (print! (item "You have local modifications in Doom's source. Discarding them..."))
          (sh! "git" "reset" "--hard" (format "origin/%s" branch))
          (sh! "git" "clean" "-ffd")))

      (sh! "git" "remote" "remove" doom-upgrade-remote)
      (unwind-protect
          (let (result)
            (or (zerop (car (sh! "git" "remote" "add" doom-upgrade-remote doom-upgrade-url)))
                (error "Failed to add %s to remotes" doom-upgrade-remote))
            (or (zerop (car (setq result (sh! "git" "fetch" "--force" "--tags" doom-upgrade-remote (format "%s:%s" branch target-remote)))))
                (error "Failed to fetch from upstream"))

            (let ((this-rev (cdr (sh! "git" "rev-parse" "HEAD")))
                  (new-rev  (cdr (sh! "git" "rev-parse" target-remote))))
              (cond
               ((and (null this-rev)
                     (null new-rev))
                (error "Failed to get revisions for %s" target-remote))

               ((equal this-rev new-rev)
                (print! (success "Doom is already up-to-date!"))
                nil)

               ((print! (item "A new version of Doom Emacs is available!\n\n  Old revision: %s (%s)\n  New revision: %s (%s)\n"
                              (substring this-rev 0 10)
                              (cdr (sh! "git" "log" "-1" "--format=%cr" "HEAD"))
                              (substring new-rev 0 10)
                              (cdr (sh! "git" "log" "-1" "--format=%cr" target-remote))))
                (let ((diff-url
                       (format "%s/compare/%s...%s"
                               doom-upgrade-url
                               this-rev
                               new-rev)))
                  (print! "Link to diff: %s" diff-url)
                  (when (and (not auto-accept-p)
                             (y-or-n-p "View the comparison diff in your browser?"))
                    (print! (item "Opened github in your browser."))
                    (browse-url diff-url)))

                (if (not (or auto-accept-p
                             (y-or-n-p "Proceed with upgrade?")))
                    (ignore (print! (error "Aborted")))
                  (print! (start "Upgrading Doom Emacs..."))
                  (print-group!
                   (doom-compile-clean)
                   (doom-cli-context-put context 'straight-recipe (doom-upgrade--get-straight-recipe))
                   (or (and (zerop (car (sh! "git" "reset" "--hard" target-remote)))
                            (equal (cdr (sh! "git" "rev-parse" "HEAD")) new-rev))
                       (error "Failed to check out %s" (substring new-rev 0 10)))))))))
        (ignore-errors
          (sh! "git" "branch" "-D" target-remote)
          (sh! "git" "remote" "remove" doom-upgrade-remote))))))

(defun doom-upgrade--working-tree-dirty-p (dir)
  (cl-destructuring-bind (success . stdout)
      (doom-call-process "git" "status" "--porcelain" "-uno")
    (if (= 0 success)
        (split-string stdout "\n" t)
      (error "Failed to check working tree in %s" dir))))

(defun doom-upgrade--get-straight-recipe ()
  (with-temp-buffer
    (insert-file-contents (doom-path doom-core-dir doom-module-packages-file))
    (when (re-search-forward "(package! straight" nil t)
      (goto-char (match-beginning 0))
      (let ((sexp (sexp-at-point)))
        (plist-put sexp :recipe
                   (eval (plist-get sexp :recipe)
                         t))))))

;;; upgrade.el ends here
