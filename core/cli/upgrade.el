;;; core/cli/upgrade.el -*- lexical-binding: t; -*-

(defcli! (upgrade up)
    ((force-p ["-f" "--force"] "Discard local changes to Doom and packages, and upgrade anyway")
     (packages-only-p ["-p" "--packages"] "Only upgrade packages, not Doom"))
  "Updates Doom and packages.

This requires that ~/.emacs.d is a git repo, and is the equivalent of the
following shell commands:

    cd ~/.emacs.d
    git pull --rebase
    bin/doom clean
    bin/doom sync -u"
  :bare t
  (let ((doom-auto-discard force-p)
        (default-directory doom-emacs-dir))
    (cond
     ((equal (replace-regexp-in-string
              "^\\(?:[^/]+/[^/]+/\\)?\\(.+\\)\\(?:~[0-9]+\\)?$" "\\1"
              (cdr (doom-call-process "git" "name-rev" "--name-only" "HEAD")))
             "develop")
      (print! (warn "Doom's primary branch has changed to 'master'. The develop branch will no\nlonger recieve updates and will eventually be deleted.\n"))
      (if (not (or force-p (y-or-n-p "Switch to the master branch?")))
          (error! "Aborting...")
        (print! (info "Switching to master branch"))
        (let ((remote
               (cdr (doom-call-process "git" "config" "--get" "branch.develop.remote"))))
          (doom-call-process "git" "config" (format "remote.%s.fetch" remote) (format "+refs/heads/*:refs/remotes/%s/*" remote))
          (doom-call-process "git" "fetch" "--append" remote "master")
          (doom-call-process "git" "checkout" "--track" (format "%s/master" remote))
          (print! (info "Reloading Doom Emacs"))
          (throw 'exit (list "doom" "upgrade" (if force-p "-f"))))))

     (packages-only-p
      (doom-cli-execute "sync" "-u")
      (print! (success "Finished upgrading Doom Emacs")))

     ((doom-cli-upgrade doom-auto-accept doom-auto-discard)
      ;; Reload Doom's CLI & libraries, in case there were any upstream changes.
      ;; Major changes will still break, however
      (print! (info "Reloading Doom Emacs"))
      (throw 'exit (list "doom" "upgrade" "-p" (if force-p "-f"))))

     ((print! "Doom is up-to-date!")
      (doom-cli-execute "sync" "-u")))))


;;
;;; library

(defvar doom-repo-url "https://github.com/hlissner/doom-emacs"
  "The git repo url for Doom Emacs.")
(defvar doom-repo-remote "_upgrade"
  "The name to use as our staging remote.")

(defun doom--working-tree-dirty-p (dir)
  (cl-destructuring-bind (success . stdout)
      (doom-call-process "git" "status" "--porcelain" "-uno")
    (if (= 0 success)
        (split-string stdout "\n" t)
      (error "Failed to check working tree in %s" dir))))

(defun doom--get-straight-recipe ()
  (with-temp-buffer
    (insert-file-contents (doom-path doom-core-dir "packages.el"))
    (when (re-search-forward "(package! straight" nil t)
      (goto-char (match-beginning 0))
      (let ((sexp (sexp-at-point)))
        (plist-put sexp :recipe
                   (eval (plist-get sexp :recipe)
                         t))))))


(defun doom-cli-upgrade (&optional auto-accept-p force-p)
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
                    (cdr (doom-call-process "git" "name-rev" "--name-only" "HEAD"))))
           (target-remote (format "%s_%s" doom-repo-remote branch)))
      (unless branch
        (error! (if (file-exists-p! ".git" doom-emacs-dir)
                    "Couldn't find Doom's .git directory. Was Doom cloned properly?"
                  "Couldn't detect what branch you're on. Is Doom detached?")))

      ;; We assume that a dirty .emacs.d is intentional and abort
      (when-let (dirty (doom--working-tree-dirty-p default-directory))
        (if (not force-p)
            (user-error! "%s\n\n%s\n\n %s"
                         (format "Refusing to upgrade because %S has been modified." (path doom-emacs-dir))
                         "Either stash/undo your changes or run 'doom upgrade -f' to discard local changes."
                         (string-join dirty "\n"))
          (print! (info "You have local modifications in Doom's source. Discarding them..."))
          (doom-call-process "git" "reset" "--hard" (format "origin/%s" branch))
          (doom-call-process "git" "clean" "-ffd")))

      (doom-call-process "git" "remote" "remove" doom-repo-remote)
      (unwind-protect
          (let (result)
            (or (zerop (car (doom-call-process "git" "remote" "add" doom-repo-remote doom-repo-url)))
                (error "Failed to add %s to remotes" doom-repo-remote))
            (or (zerop (car (setq result (doom-call-process "git" "fetch" "--force" "--tags" doom-repo-remote (format "%s:%s" branch target-remote)))))
                (error "Failed to fetch from upstream"))

            (let ((this-rev (cdr (doom-call-process "git" "rev-parse" "HEAD")))
                  (new-rev  (cdr (doom-call-process "git" "rev-parse" target-remote))))
              (cond
               ((and (null this-rev)
                     (null new-rev))
                (error "Failed to get revisions for %s" target-remote))

               ((equal this-rev new-rev)
                (print! (success "Doom is already up-to-date!"))
                nil)

               ((print! (info "A new version of Doom Emacs is available!\n\n  Old revision: %s (%s)\n  New revision: %s (%s)\n"
                              (substring this-rev 0 10)
                              (cdr (doom-call-process "git" "log" "-1" "--format=%cr" "HEAD"))
                              (substring new-rev 0 10)
                              (cdr (doom-call-process "git" "log" "-1" "--format=%cr" target-remote))))
                (let ((diff-url
                       (format "%s/compare/%s...%s"
                               doom-repo-url
                               this-rev
                               new-rev)))
                  (print! "Link to diff: %s" diff-url)
                  (when (and (not auto-accept-p)
                             (y-or-n-p "View the comparison diff in your browser?"))
                    (print! (info "Opened github in your browser."))
                    (browse-url diff-url)))

                (if (not (or auto-accept-p
                             (y-or-n-p "Proceed with upgrade?")))
                    (ignore (print! (error "Aborted")))
                  (print! (start "Upgrading Doom Emacs..."))
                  (print-group!
                   (doom-clean-byte-compiled-files)
                   (let ((straight-recipe (doom--get-straight-recipe)))
                     (or (and (zerop (car (doom-call-process "git" "reset" "--hard" target-remote)))
                              (equal (cdr (doom-call-process "git" "rev-parse" "HEAD")) new-rev))
                         (error "Failed to check out %s" (substring new-rev 0 10)))
                     ;; HACK It's messy to use straight to upgrade straight, due
                     ;;      to the potential for backwards incompatibility, so
                     ;;      we staticly check if Doom's `package!' declaration
                     ;;      for straight has changed. If it has, delete
                     ;;      straight so 'doom upgrade's second stage will
                     ;;      install the new version for us.
                     ;;
                     ;;      Clumsy, but a better solution is in the works.
                     (unless (equal straight-recipe (doom--get-straight-recipe))
                       (print! (info "Preparing straight for an update"))
                       (delete-directory (doom-path straight-base-dir "straight/repos/straight.el")
                                         'recursive)))
                   (print! (info "%s") (cdr result))
                   t))))))
        (ignore-errors
          (doom-call-process "git" "branch" "-D" target-remote)
          (doom-call-process "git" "remote" "remove" doom-repo-remote))))))
