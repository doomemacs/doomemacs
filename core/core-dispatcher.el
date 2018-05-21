;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(load! autoload/packages)
(load! autoload/modules)
(load! autoload/debug)
(load! autoload/message)


;;
;; Dispatcher
;;

(defvar doom-dispatch-command-alist ()
  "TODO")

(defvar doom-dispatch-alias-alist ()
  "TODO")

(defun doom--dispatch-format (desc &optional short)
  (if (equal desc "TODO")
      (format! (yellow "TODO"))
    (with-temp-buffer
      (let ((fill-column 72))
        (insert desc)
        (goto-char (point-min))
        (while (re-search-forward "\n\n[^ \n]" nil t)
          (fill-paragraph)))
      (if (not short)
          (buffer-string)
        (goto-char (point-min))
        (buffer-substring-no-properties
         (line-beginning-position)
         (line-end-position))))))

(defun doom--dispatch-help (&optional command desc &rest args)
  "TODO"
  (if command
      (princ (doom--dispatch-format desc))
    (print! (bold "%-10s\t%s\t%s" "Command:" "Alias" "Description"))
    (dolist (spec (sort doom-dispatch-command-alist
                        (lambda (x y) (string-lessp (car x) (car y)))))
      (cl-destructuring-bind (command &key desc _body) spec
        (let ((alias (car (rassq command doom-dispatch-alias-alist))))
          (print! "  %-10s\t%s\t%s"
                  command (or alias "")
                  (doom--dispatch-format desc t)))))))

(defun doom-dispatch (args)
  "TODO"
  (let ((help (equal (car args) "help")))
    (if help (pop args))
    (cl-destructuring-bind (command &key desc body)
        (let ((sym (intern (car args))))
          (or (assq sym doom-dispatch-command-alist)
              (assq (cdr (assq sym doom-dispatch-alias-alist)) doom-dispatch-command-alist)
              (error "Invalid command: %s" (car args))))
      (if help
          (apply #'doom--dispatch-help command desc (cdr args))
        (funcall body (cdr args))))))

;; FIXME Clumsy way of registering commands, refactor!
(defmacro def-dispatcher! (command desc &rest body)
  "TODO"
  (declare (doc-string 2))
  (let* ((command (doom-enlist command))
         (cmd (car command))
         (alias (car (cdr command))))
    `(progn
       ,(when alias
          `(map-put doom-dispatch-alias-alist ',alias ',cmd))
       (map-put doom-dispatch-command-alist
                ',cmd (list :desc ,desc :body (lambda (args) ,@body))))))

;;
(def-dispatcher! run
  "Run Doom Emacs from bin/doom's parent directory.

All arguments are passed on to Emacs (except for -p and -e).

  doom run
  doom run -nw init.el

Warning, this is for convenience and testing purposes, Doom will not run its
best or fastest when started in this manner.")

(def-dispatcher! (doctor doc)
  "Checks for issues with your current Doom config.")

(def-dispatcher! (help h)
  "Look up additional information about a command.")

;;
(def-dispatcher! quickstart
  "TODO"
  (doom//quickstart))

(def-dispatcher! (install i)
  "Installs requested plugins that aren't installed."
  (doom-initialize)
  (when (doom//packages-install)
    (doom//reload-autoloads)))

(def-dispatcher! (update u)
  "Checks for and updates outdated plugins."
  (doom-initialize)
  (when (doom//packages-update)
    (doom//reload-autoloads)))

(def-dispatcher! (autoremove r)
  "Removes orphaned plugins."
  (doom-initialize)
  (when (doom//packages-autoremove)
    (doom//reload-autoloads)))

(def-dispatcher! (autoloads a)
  "Regenerates Doom's autoloads file.

This file tells Emacs where to find your module's autoloaded functions and
plugins."
  (doom-initialize)
  (doom//reload-autoloads))

(def-dispatcher! (upgrade up)
  "Checks out the latest Doom on this branch."
  (doom//upgrade))

(def-dispatcher! (compile c)
  "Byte-compiles your config or selected modules.

  compile [TARGETS...]
  compile :core :private lang/python
  compile feature lang

Accepts :core, :private and :plugins as special arguments, indicating you want
to byte-compile Doom's core files, your private config or your ELPA plugins,
respectively."
  (doom//byte-compile args))

(def-dispatcher! (recompile cc)
  "Re-byte-compiles outdated *.elc files."
  (doom//byte-compile args 'recompile))

(def-dispatcher! clean
  "Delete all *.elc files."
  (doom//clean-byte-compiled-files))

(def-dispatcher! test
  "Run Doom unit tests."
  (load! autoload/test)
  (doom//run-tests args))

(def-dispatcher! info
  "Output system info in markdown for bug reports."
  (doom//info))

(def-dispatcher! (version v)
  "Reports the version of Doom and Emacs."
  (doom//version))

(def-dispatcher! (refresh re)
  "Refresh Doom.

This is the equivalent of running autoremove, install, autoloads, then
recompile. Run this whenever you:

  1. Modify your `doom!' block,
  2. Add or remove `package!' blocks to your config,
  3. Add or remove autoloaded functions in module autoloaded files.
  4. Update Doom outside of Doom (e.g. with git)"
  (doom-initialize)
  (let (reload-p)
    (when (let* ((doom--inhibit-reload t)
                 (autoremove-p (doom//packages-autoremove))
                 (install-p (doom//packages-install)))
            (or autoremove-p install-p))
      (doom//reload))
    (doom//byte-compile nil 'recompile)))


;;
;; Quality of Life Commands
;;

(defvar doom-remote "origin"
  "TODO")

(defun doom//upgrade ()
  "TODO"
  (declare (interactive-only t))
  (interactive)
  (let ((core-file (expand-file-name "init.el" doom-core-dir))
        (branch (vc-git--symbolic-ref core-file))
        (default-directory doom-emacs-dir))
    (unless (file-exists-p core-file)
      (error "Couldn't find %s, was Doom cloned properly?"
             (abbreviate-file-name core-file)))
    (unless branch
      (error "Couldn't detect what branch you're using. Is %s a repo?"
             (abbreviate-file-name doom-emacs-dir)))
    (unless (eq (vc-state core-file 'Git) 'up-to-date)
      (user-error "Doom has been modified; refusing to upgrade. Stash or undo your changes"))
    (with-temp-buffer
      (let ((buf (current-buffer)))
        (when (zerop (process-file "git" nil buf nil
                                   "fetch" "--tags" doom-remote branch))
          (let ((current-rev (vc-git-working-revision core-file))
                (rev (shell-command-to-string (format "git rev-parse %s/%s" doom-remote branch))))
            (unless rev
              (error "Couldn't detect Doom's version. Is %s a repo?"
                     (abbreviate-file-name doom-emacs-dir)))
            (if (equal current-rev rev)
                (message "Doom is up to date!")

              (when (or (getenv "YES")
                        (y-or-n-p "Doom is out of date, update?"))
                (unless (zerop (process-file "git" nil buf nil
                                             "checkout" (format "%s/%s" doom-remote branch)))
                  (error "An error occurred while checking out the latest commit"))
                (when (file-exists-p (byte-compile-dest-file core-file))
                  (message "Your config is byte-compiled, removing byte-compiled files")
                  (doom//clean-byte-compiled-files))
                (doom//reload)
                (message "Done! Please restart Emacs for changes to take effect")))))))))

(defun doom//quickstart ()
  "TODO"
  (declare (interactive-only t))
  (interactive)
  (let ((short-private-dir (abbreviate-file-name doom-private-dir)))
    (when (file-directory-p doom-private-dir)
      (error "%s already exists! Aborting." short-private-dir))
    (message "Creating %s directory" short-private-dir)
    (make-directory doom-private-dir)
    (let ((init-file (expand-file-name "init.el" doom-private-dir)))
      (if (not (file-exists-p init-file))
          (message "%sinit.el already exists. Skipping.")
        (message "Copying init.example.el to %s" short-private-dir)
        (copy-file (expand-file-name "init.example.el" doom-emacs-dir)
                   init-file)))
    (let ((config-file (expand-file-name "config.el" doom-private-dir)))
      (if (file-exists-p config-file)
          (with-temp-file config-file
            (insert ""))
        (message "%sconfig.el already exists. Skipping."))))
  (doom-initialize)
  (let* ((doom--inhibit-reload t)
         (autoremove-p (doom//packages-autoremove))
         (install-p (doom//packages-install)))
    (or autoremove-p install-p))
  (doom//reload-autoloads)
  (message "\n\nDone! Doom Emacs is ready.\n")
  (message "Remember to run M-x all-the-icons-install-fonts after starting Emacs for the first time."))

(provide 'core-dispatcher)
;;; core-dispatcher.el ends here
