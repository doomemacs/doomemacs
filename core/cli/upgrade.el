;;; core/cli/upgrade.el -*- lexical-binding: t; -*-

(defcli! (upgrade up) (&rest args)
  "Updates Doom and packages.

This requires that ~/.emacs.d is a git repo, and is the equivalent of the
following shell commands:

    cd ~/.emacs.d
    git pull --rebase
    bin/doom clean
    bin/doom refresh
    bin/doom update

Switches:
  -t/--timeout TTL   Seconds until a thread is timed out (default: 45)
  --threads N        How many threads to use (default: 8)"
  (and (doom-upgrade doom-auto-accept
                     (or (member "-f" args)
                         (member "--force" args)))
       (doom-packages-update
        doom-auto-accept
        (when-let (threads (cadr (member "--threads" args)))
          (string-to-number threads))
        (when-let (timeout (cadr (or (member "--timeout" args)
                                     (member "-t" args))))
          (string-to-number timeout)))
       (doom-reload-package-autoloads 'force-p)))


;;
;;; library

(defvar doom-repo-url "https://github.com/hlissner/doom-emacs"
  "The git repo url for Doom Emacs.")
(defvar doom-repo-remote "_upgrade"
  "The name to use as our staging remote.")

(defun doom--working-tree-dirty-p (dir)
  (cl-destructuring-bind (success . stdout)
      (doom-sh "git" "status" "--porcelain" "-uno")
    (if (= 0 success)
        (string-match-p "[^ \t\n]" (buffer-string))
      (error "Failed to check working tree in %s" dir))))


(defun doom-upgrade (&optional auto-accept-p force-p)
  "Upgrade Doom to the latest version non-destructively."
  (require 'vc-git)
  (let ((default-directory doom-emacs-dir)
        process-file-side-effects)
    (print! (start "Preparing to upgrade Doom Emacs and its packages..."))

    (let* ((branch (vc-git--symbolic-ref doom-emacs-dir))
           (target-remote (format "%s/%s" doom-repo-remote branch)))
      (unless branch
        (error! (if (file-exists-p! ".git" doom-emacs-dir)
                    "Couldn't find Doom's .git directory. Was Doom cloned properly?"
                  "Couldn't detect what branch you're on. Is Doom detached?")))

      ;; We assume that a dirty .emacs.d is intentional and abort
      (when (doom--working-tree-dirty-p default-directory)
        (if (not force-p)
            (user-error! "%s\n\n%s"
                         (format "Refusing to upgrade because %S has been modified." (path doom-emacs-dir))
                         "Either stash/undo your changes or run 'doom upgrade -f' to discard local changes.")
          (print! (info "You have local modifications in Doom's source. Discarding them..."))
          (doom-sh "git" "reset" "--hard" (format "origin/%s" branch))
          (doom-sh "git" "clean" "-ffd")))

      (doom-sh "git" "remote" "remove" doom-repo-remote)
      (unwind-protect
          (progn
            (or (zerop (car (doom-sh "git" "remote" "add" doom-repo-remote doom-repo-url)))
                (error "Failed to add %s to remotes" doom-repo-remote))
            (or (zerop (car (doom-sh "git" "fetch" "--tags" doom-repo-remote branch)))
                (error "Failed to fetch from upstream"))

            (let ((this-rev (vc-git--rev-parse "HEAD"))
                  (new-rev  (vc-git--rev-parse target-remote)))
              (cond
               ((and (null this-rev)
                     (null new-rev))
                (error "Failed to get revisions for %s" target-remote))

               ((equal this-rev new-rev)
                (print! (success "Doom is already up-to-date!"))
                t)

               ((print! (info "A new version of Doom Emacs is available!\n\n  Old revision: %s (%s)\n  New revision: %s (%s)\n"
                              (substring this-rev 0 10)
                              (cdr (doom-sh "git" "log" "-1" "--format=%cr" "HEAD"))
                              (substring new-rev 0 10)
                              (cdr (doom-sh "git" "log" "-1" "--format=%cr" target-remote))))

                (when (and (not auto-accept-p)
                           (y-or-n-p "View the comparison diff in your browser?"))
                  (print! (info "Opened github in your browser."))
                  (browse-url (format "https://github.com/hlissner/doom-emacs/compare/%s...%s"
                                      this-rev
                                      new-rev)))

                (if (not (or auto-accept-p
                             (y-or-n-p "Proceed with upgrade?")))
                    (ignore (print! (error "Aborted")))
                  (print! (start "Upgrading Doom Emacs..."))
                  (print-group!
                   (doom-clean-byte-compiled-files)
                   (unless (and (zerop (car (doom-sh "git" "reset" "--hard" target-remote)))
                                (equal (vc-git--rev-parse "HEAD") new-rev))
                     (error "Failed to check out %s" (substring new-rev 0 10)))
                   (print! (success "Finished upgrading Doom Emacs")))
                  (doom-delete-autoloads-file doom-autoload-file)
                  (doom-delete-autoloads-file doom-package-autoload-file)
                  (doom-cli-refresh "-f")
                  t)

                (print! (success "Done! Restart Emacs for changes to take effect."))))))
        (ignore-errors
          (doom-sh "git" "remote" "remove" doom-repo-remote))))))
