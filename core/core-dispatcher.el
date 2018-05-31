;;; -*- lexical-binding: t; no-byte-compile: t; -*-

;; Eagerly load these libraries because this module may be loaded in a session
;; that hasn't been fully initialized (where autoloads files haven't been
;; generated or `load-path' populated).
(load! "autoload/packages")
(load! "autoload/modules")
(load! "autoload/debug")
(load! "autoload/message")


;;
;; Dispatcher API
;;

(defvar doom-auto-accept (getenv "YES")
  "If non-nil, Doom will auto-accept any confirmation prompts during batch
commands like `doom//packages-install', `doom//packages-update' and
`doom//packages-autoremove'.")

(defconst doom--dispatch-command-alist ())
(defconst doom--dispatch-alias-alist ())

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
  "Display help documentation for a dispatcher command. If COMMAND and DESC are
omitted, show all available commands, their aliases and brief descriptions."
  (if command
      (princ (doom--dispatch-format desc))
    (print! (bold "%-10s\t%s\t%s" "Command:" "Alias" "Description"))
    (dolist (spec (sort doom--dispatch-command-alist
                        (lambda (x y) (string-lessp (car x) (car y)))))
      (cl-destructuring-bind (command &key desc _body) spec
        (let ((aliases (cl-loop for (alias . cmd) in doom--dispatch-alias-alist
                                if (eq cmd command)
                                collect (symbol-name alias))))
          (print! "  %-10s\t%s\t%s"
                  command (if aliases (string-join aliases ",") "")
                  (doom--dispatch-format desc t)))))))

(defun doom-dispatch (args)
  "Invoke a dispatcher command and pass ARGS to it."
  (let ((help (equal (car args) "help")))
    (if help (pop args))
    (cl-destructuring-bind (command &key desc body)
        (let ((sym (intern (car args))))
          (or (assq sym doom--dispatch-command-alist)
              (assq (cdr (assq sym doom--dispatch-alias-alist)) doom--dispatch-command-alist)
              (error "Invalid command: %s" (car args))))
      (if help
          (apply #'doom--dispatch-help command desc (cdr args))
        (funcall body (cdr args))))))

;; FIXME Clumsy way of registering commands, refactor!
(defmacro def-dispatcher! (command desc &rest body)
  "Define a dispatcher command. COMMAND is a symbol or a list of symbols
representing the aliases for this command. DESC is a string description. The
first line should be short (under 60 letters), as it will be displayed for
bin/doom help.

BODY will be run when this dispatcher is called."
  (declare (doc-string 2))
  (let* ((command (doom-enlist command))
         (cmd (car command))
         (aliases (cdr command)))
    `(progn
       ,(when aliases
          `(dolist (alias ',aliases)
             (map-put doom--dispatch-alias-alist alias ',cmd)))
       (map-put doom--dispatch-command-alist
                ',cmd (list :desc ,desc
                            ;; FIXME Implicit args var; ew
                            :body (lambda (args) ,@body))))))


;;
;; Dispatch commands
;;

;; Dummy dispatchers (no-op because they're handled especially)
(def-dispatcher! run
  "Run Doom Emacs from bin/doom's parent directory.

All arguments are passed on to Emacs (except for -p and -e).

  doom run
  doom run -nw init.el

WARNING: this command exists for convenience and testing. Doom will suffer
additional overhead for be started this way. For the best performance, it
is best to run Doom out of ~/.emacs.d and ~/.doom.d.")

(def-dispatcher! (doctor doc)
  "Checks for issues with your current Doom config.")

(def-dispatcher! (help h)
  "Look up additional information about a command.")

;; Real dispatchers
(def-dispatcher! (quickstart qs)
  "Quickly deploy a private module and Doom.

This deploys a barebones config to ~/.doom.d. The destination can be changed
with the -p option, e.g.

  doom -p ~/.config/doom quickstart

This command will refuse to overwrite the private directory if it already
exists."
  (doom//quickstart))

(def-dispatcher! (install i)
  "Installs requested plugins that aren't installed."
  (doom//reload-doom-autoloads)
  (when (doom//packages-install doom-auto-accept)
    (doom//reload-package-autoloads)))

(def-dispatcher! (update u)
  "Checks for and updates outdated plugins."
  (doom//reload-doom-autoloads)
  (when (doom//packages-update doom-auto-accept)
    (doom//reload-package-autoloads)))

(def-dispatcher! (autoremove r)
  "Removes orphaned plugins."
  (doom//reload-doom-autoloads)
  (when (doom//packages-autoremove doom-auto-accept)
    (doom//reload-package-autoloads)))

(def-dispatcher! (autoloads a)
  "Regenerates Doom's autoloads file.

This file tells Emacs where to find your module's autoloaded functions and
plugins."
  (doom//reload-autoloads nil 'force))

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

(def-dispatcher! (recompile rc)
  "Re-byte-compiles outdated *.elc files."
  (doom//byte-compile args 'recompile))

(def-dispatcher! clean
  "Delete all *.elc files."
  (doom//clean-byte-compiled-files))

(def-dispatcher! test
  "Run Doom unit tests."
  (load! "autoload/test")
  (doom//run-tests args))

(def-dispatcher! info
  "Output system info in markdown for bug reports."
  (doom/info))

(def-dispatcher! (version v)
  "Reports the version of Doom and Emacs."
  (doom/version))

(def-dispatcher! (refresh re)
  "Refresh Doom. Same as autoremove+install+autoloads.

This is the equivalent of running autoremove, install, autoloads, then
recompile. Run this whenever you:

  1. Modify your `doom!' block,
  2. Add or remove `package!' blocks to your config,
  3. Add or remove autoloaded functions in module autoloaded files.
  4. Update Doom outside of Doom (e.g. with git)"
  (doom//refresh))


;;
;; Quality of Life Commands
;;

(defvar doom-repo-url "https://github.com/hlissner/doom-emacs"
  "TODO")
(defvar doom-repo-remote "_upgrade"
  "TODO")

(defun doom--working-tree-dirty-p (dir)
  (with-temp-buffer
    (let ((default-directory dir))
      (if (zerop (process-file "git" nil (current-buffer) nil
                               "status" "--porcelain"))
          (string-match-p "[^ \t\n]" (buffer-string))
        (error "Failed to check working tree in %s" dir)))))

(defun doom//refresh ()
  "Ensure Doom is in a working state by checking autoloads and packages, and
recompiling any changed compiled files. This is the shotgun solution to most
problems with doom."
  (interactive)
  (doom//reload-doom-autoloads)
  (unwind-protect
      (progn (doom//packages-autoremove)
             (doom//packages-install))
    (doom//reload-package-autoloads)
    (doom//byte-compile nil 'recompile)))

(defun doom//upgrade ()
  "Upgrade Doom to the latest version."
  (interactive)
  (require 'vc-git)
  (let* ((gitdir (expand-file-name ".git" doom-emacs-dir))
         (branch (vc-git--symbolic-ref doom-emacs-dir))
         (default-directory doom-emacs-dir))
    (unless (file-exists-p gitdir)
      (error "Couldn't find %s, was Doom cloned properly?"
             (abbreviate-file-name gitdir)))
    (unless branch
      (error "Couldn't detect what branch you're using. Is %s a repo?"
             (abbreviate-file-name doom-emacs-dir)))
    (when (doom--working-tree-dirty-p doom-emacs-dir)
      (user-error "Refusing to upgrade because Doom has been modified. Stash or undo your changes"))
    (with-temp-buffer
      (let ((buf (current-buffer)))
        (process-file "git" nil buf nil "remote" "remove" doom-repo-remote)
        (when (and (zerop (process-file "git" nil buf nil "remote" "add"
                                        doom-repo-remote doom-repo-url))
                   (zerop (process-file "git" nil buf nil
                                        "fetch" "--tags" doom-repo-remote branch)))
          (let ((current-rev (vc-git-working-revision doom-emacs-dir))
                (rev (string-trim (shell-command-to-string (format "git rev-parse %s/%s" doom-repo-remote branch)))))
            (unless rev
              (error "Couldn't detect Doom's version. Is %s a repo?"
                     (abbreviate-file-name doom-emacs-dir)))
            (if (equal current-rev rev)
                (message "Doom is up to date!")
              (message "Doom is out of date.\n\n  Old rev: %s\n  New rev: %s\n"
                       current-rev rev)
              (if (not (or doom-auto-accept
                           (y-or-n-p "Proceed?")))
                  (error "Aborted")
                (message "Removing byte-compiled files from your config (if any)")
                (doom//clean-byte-compiled-files)
                (unless (zerop (process-file "git" nil buf nil "reset" "--hard"
                                             (format "%s/%s" doom-repo-remote branch)))
                  (error "An error occurred while checking out the latest commit\n\n%s"
                         (buffer-string)))
                (unless (equal (vc-git-working-revision doom-emacs-dir) rev)
                  (error "Failed to checkout latest commit.\n\n%s" (buffer-string)))
                (doom//refresh)
                (message "Done! Please restart Emacs for changes to take effect")))))))))

(defun doom//quickstart ()
  "Quickly deploy a private module and Doom.

This deploys a barebones config to `doom-private-dir', installs all missing
packages and regenerates the autoloads file."
  (interactive)
  (let ((short-private-dir (abbreviate-file-name doom-private-dir)))
    (if (file-directory-p doom-private-dir)
        (print! (yellow "%s directory already exists. Skipping." short-private-dir))
      (print! "Creating %s" short-private-dir)
      (make-directory doom-private-dir t)
      (print! (green "Done!")))
    (let ((init-file (expand-file-name "init.el" doom-private-dir)))
      (if (file-exists-p init-file)
          (print! (yellow "%sinit.el already exists. Skipping." short-private-dir))
        (print! "Copying init.example.el to %s" short-private-dir)
        (copy-file (expand-file-name "init.example.el" doom-emacs-dir)
                   init-file)
        (print! (green "Done!"))))
    (let ((config-file (expand-file-name "config.el" doom-private-dir)))
      (if (file-exists-p config-file)
          (print! "%sconfig.el already exists. Skipping." short-private-dir)
        (print! "Deploying empty config.el file in %s" short-private-dir)
        (with-temp-file config-file (insert ""))
        (print! (green "Done!")))))
  (print! "Installing plugins")
  (doom//packages-install)
  (print! "Regenerating autoloads files")
  (doom//reload-autoloads nil 'force-p)
  (print! (bold (green "\nFinished! Doom is ready to go!\n")))
  (with-temp-buffer
    (doom-template-insert "QUICKSTART_INTRO")
    (print! (buffer-string))))

(provide 'core-dispatcher)
;;; core-dispatcher.el ends here
