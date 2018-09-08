;;; core/cli/upgrade.el -*- lexical-binding: t; -*-

(dispatcher! (upgrade up) (doom-upgrade)
  "Checks out the latest Doom on this branch.")


;;
;; Quality of Life Commands

(defvar doom-repo-url "https://github.com/hlissner/doom-emacs"
  "TODO")
(defvar doom-repo-remote "_upgrade"
  "TODO")

(defun doom--working-tree-dirty-p (dir)
  (with-temp-buffer
    (let ((default-directory dir))
      (if (zerop (process-file "git" nil (current-buffer) nil
                               "status" "--porcelain" "-uno"))
          (string-match-p "[^ \t\n]" (buffer-string))
        (error "Failed to check working tree in %s" dir)))))

(defun doom-upgrade ()
  "Upgrade Doom to the latest version non-destructively."
  (require 'vc-git)
  (let* ((gitdir (expand-file-name ".git" doom-emacs-dir))
         (branch (vc-git--symbolic-ref doom-emacs-dir))
         (default-directory doom-emacs-dir))
    (unless (file-exists-p gitdir)
      (error "Couldn't find %s. Was Doom cloned properly?"
             (abbreviate-file-name gitdir)))
    (unless branch
      (error "Couldn't detect what branch you're using. Is Doom detached?"))
    (when (doom--working-tree-dirty-p doom-emacs-dir)
      (user-error "Refusing to upgrade because Doom has been modified. Stash or undo your changes"))
    (with-temp-buffer
      (let ((buf (current-buffer)))
        (condition-case-unless-debug e
            (progn
              (process-file "git" nil buf nil "remote" "remove" doom-repo-remote)
              (unless (zerop (process-file "git" nil buf nil "remote" "add"
                                           doom-repo-remote doom-repo-url))
                (error "Failed to add %s to remotes" doom-repo-remote))
              (unless (zerop (process-file "git" nil buf nil "fetch" "--tags"
                                           doom-repo-remote branch))
                (error "Failed to fetch from upstream"))
              (let ((current-rev (vc-git-working-revision doom-emacs-dir))
                    (rev (string-trim (shell-command-to-string (format "git rev-parse %s/%s" doom-repo-remote branch)))))
                (unless rev
                  (error "Couldn't detect Doom's version. Is %s a repo?"
                         (abbreviate-file-name doom-emacs-dir)))
                (when (equal current-rev rev)
                  (user-error "Doom is up to date!"))
                (message "Updates for Doom are available!\n\n  Old revision: %s\n  New revision: %s\n"
                         current-rev rev)
                (message "Comparision diff: https://github.com/hlissner/doom-emacs/compare/%s...%s\n"
                         (substring current-rev 0 10) (substring rev 0 10))
                ;; TODO Display newsletter diff
                (unless (or doom-auto-accept (y-or-n-p "Proceed?"))
                  (user-error "Aborted"))
                (message "Removing byte-compiled files from your config (if any)")
                (doom-clean-byte-compiled-files)
                (unless (zerop (process-file "git" nil buf nil "reset" "--hard"
                                             (format "%s/%s" doom-repo-remote branch)))
                  (error "An error occurred while checking out the latest commit\n\n%s"
                         (buffer-string)))
                (unless (equal (vc-git-working-revision doom-emacs-dir) rev)
                  (error "Failed to checkout latest commit.\n\n%s" (buffer-string)))
                (doom-refresh 'force)
                (message "Done! Please restart Emacs for changes to take effect")))
          (user-error
           (message "%s Aborting." (error-message-string e)))
          (error
           (message "There was an unexpected error.\n\n%s\n\nOutput:\n%s"
                    (car e)
                    (buffer-string))))))))
