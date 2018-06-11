;;; -*- lexical-binding: t; no-byte-compile: t; -*-

;; Eagerly load these libraries because this module may be loaded in a session
;; that hasn't been fully initialized (where autoloads files haven't been
;; generated or `load-path' populated).
(load! "autoload/packages")
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
       (line-end-position)))))

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
                               "status" "--porcelain" "-uno"))
          (string-match-p "[^ \t\n]" (buffer-string))
        (error "Failed to check working tree in %s" dir)))))

(defun doom//refresh ()
  "Ensure Doom is in a working state by checking autoloads and packages, and
recompiling any changed compiled files. This is the shotgun solution to most
problems with doom."
  (interactive)
  (doom//reload-doom-autoloads)
  (unwind-protect
      (progn (ignore-errors (doom//packages-autoremove))
             (ignore-errors (doom//packages-install)))
    (doom//reload-package-autoloads)
    (doom//byte-compile nil 'recompile)))

(defun doom//upgrade ()
  "Upgrade Doom to the latest version non-destructively."
  (interactive)
  (require 'vc-git)
  (let* ((gitdir (expand-file-name ".git" doom-emacs-dir))
         (branch (vc-git--symbolic-ref doom-emacs-dir))
         (default-directory doom-emacs-dir))
    (unless (file-exists-p gitdir)
      (error "Couldn't find %s. Was Doom cloned properly?"
             (abbreviate-file-name gitdir)))
    (unless branch
      (error "Couldn't detect what branch you're using. Is Doom detached?"
             (abbreviate-file-name doom-emacs-dir)))
    (when (doom--working-tree-dirty-p doom-emacs-dir)
      (user-error "Refusing to upgrade because Doom has been modified. Stash or undo your changes"))
    (with-temp-buffer
      (let ((buf (current-buffer)))
        (condition-case e
            (progn
              (process-file "git" nil buf nil "remote" "remove" doom-repo-remote)
              (unless (zerop (process-file "git" nil buf nil "remote" "add"
                                           doom-repo-remote doom-repo-url))
                (error "Failed to add %s to remotes" doom-repo-remote))
              (unless (zerop (process-file "git" nil buf nil "fetch" "--tags"
                                           doom-repo-remote branch))
                (error "Failed to fetch from upstream"))
              (when
                  (let ((current-rev (vc-git-working-revision doom-emacs-dir))
                        (rev (string-trim (shell-command-to-string (format "git rev-parse %s/%s" doom-repo-remote branch)))))
                    (unless rev
                      (error "Couldn't detect Doom's version. Is %s a repo?"
                             (abbreviate-file-name doom-emacs-dir)))
                    (when (equal current-rev rev)
                      (user-error "Doom is up to date!"))
                    (message "Updates for Doom are available!\n\n  Old revision: %s\n  New revision: %s\n"
                             current-rev rev)
                    ;; TODO Display newsletter diff
                    (unless (or doom-auto-accept (y-or-n-p "Proceed?"))
                      (error "Aborted"))
                    (message "Removing byte-compiled files from your config (if any)")
                    (doom//clean-byte-compiled-files)
                    (unless (zerop (process-file "git" nil buf nil "reset" "--hard"
                                                 (format "%s/%s" doom-repo-remote branch)))
                      (error "An error occurred while checking out the latest commit\n\n%s"
                             (buffer-string)))
                    (unless (equal (vc-git-working-revision doom-emacs-dir) rev)
                      (error "Failed to checkout latest commit.\n\n%s" (buffer-string)))
                    (doom//refresh)
                    (message "Done! Please restart Emacs for changes to take effect"))))
          (user-error
           (message "%s Aborting." (error-message-string e)))
          (error
           (message "There was an unexpected error.\n\n%s -> %s\n\nOutput:\n%s"
                    (car e)
                    (buffer-string))))))))

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


;;
;; Autoload file generation
;;

(defvar doom-autoload-excluded-packages '(marshal gh)
  "Packages that have silly or destructive autoload files that try to load
everyone in the universe and their dog, causing errors that make babies cry. No
one wants that.")

(defun doom-delete-autoloads-file (file)
  "Delete FILE (an autoloads file), and delete the accompanying *.elc file, if
it exists."
  (cl-check-type file string)
  (when (file-exists-p file)
    (delete-file file)
    (ignore-errors (delete-file (byte-compile-dest-file file)))
    (message "Deleted old %s" (file-name-nondirectory file))))

(defun doom--server-load (file)
  (require 'server)
  (when (server-running-p)
    (server-eval-at server-name `(load-file ,(byte-compile-dest-file file)))))

(defun doom--byte-compile-file (file)
  (let ((short-name (file-name-nondirectory file))
        (byte-compile-dynamic-docstrings t))
    (condition-case-unless-debug ex
        (when (byte-compile-file file)
          (load (byte-compile-dest-file file) nil t)
          (unless noninteractive
            (message "Finished compiling %s" short-name)))
      ('error
       (doom-delete-autoloads-file file)
       (error "Error in %s: %s -- %s"
              short-name
              (car ex) (error-message-string ex))))))

(defun doom//reload-autoloads (&optional file force-p)
  "Reloads FILE (an autoload file), if it needs reloading.

FILE should be one of `doom-autoload-file' or `doom-package-autoload-file'. If
it is nil, it will try to reload both. If FORCE-P (universal argument) do it
even if it doesn't need reloading!"
  (interactive
   (list nil current-prefix-arg))
  (or (null file)
      (stringp file)
      (signal 'wrong-type-argument (list 'stringp file)))
  (cond ((equal file doom-autoload-file)
         (doom//reload-doom-autoloads force-p))
        ((equal file doom-package-autoload-file)
         (doom//reload-package-autoloads force-p))
        ((progn
           (doom//reload-doom-autoloads force-p)
           (doom//reload-package-autoloads force-p)))))

(defvar generated-autoload-load-name)
(defun doom//reload-doom-autoloads (&optional force-p)
  "Refreshes the autoloads.el file, specified by `doom-autoload-file', if
necessary (or if FORCE-P is non-nil).

It scans and reads core/autoload/*.el, modules/*/*/autoload.el and
modules/*/*/autoload/*.el, and generates `doom-autoload-file'. This file tells
Emacs where to find lazy-loaded functions.

This should be run whenever your `doom!' block, or a module autoload file, is
modified."
  (interactive)
  (let ((default-directory doom-emacs-dir)
        (targets
         (file-expand-wildcards
          (expand-file-name "autoload/*.el" doom-core-dir))))
    (dolist (path (doom-module-load-path))
      (let ((auto-dir  (expand-file-name "autoload" path))
            (auto-file (expand-file-name "autoload.el" path)))
        (when (file-exists-p auto-file)
          (push auto-file targets))
        (when (file-directory-p auto-dir)
          (dolist (file (doom-files-in auto-dir :match "\\.el$" :full t))
            (push file targets)))))
    (if (and (not force-p)
             (not doom-emacs-changed-p)
             (file-exists-p doom-autoload-file)
             (not (file-newer-than-file-p (expand-file-name "init.el" doom-private-dir)
                                          doom-autoload-file))
             (not (cl-loop for file in targets
                           if (file-newer-than-file-p file doom-autoload-file)
                           return t)))
        (ignore (print! (green "Doom core autoloads is up-to-date"))
                (doom-initialize-autoloads doom-autoload-file))
      (doom-delete-autoloads-file doom-autoload-file)
      ;; in case the buffer is open somewhere and modified
      (when-let* ((buf (find-buffer-visiting doom-autoload-file)))
        (with-current-buffer buf
          (set-buffer-modified-p nil))
        (kill-buffer buf))
      (message "Generating new autoloads.el")
      (dolist (file (nreverse targets))
        (let* ((file (file-truename file))
               (generated-autoload-load-name (file-name-sans-extension file))
               (noninteractive (not doom-debug-mode)))
          (print!
           (cond ((not (doom-file-cookie-p file))
                  "⚠ Ignoring %s")
                 ((update-file-autoloads file nil doom-autoload-file)
                  (yellow "✕ Nothing in %%s"))
                 ((green "✓ Scanned %%s")))
           (if (file-in-directory-p file default-directory)
               (file-relative-name file)
             (abbreviate-file-name file)))))
      (make-directory (file-name-directory doom-autoload-file) t)
      (let ((buf (find-file-noselect doom-autoload-file t))
            case-fold-search)
        (unwind-protect
            (with-current-buffer buf
              (goto-char (point-min))
              (insert ";;; -*- lexical-binding:t -*-\n"
                      ";; This file is autogenerated by `doom//reload-doom-autoloads', DO NOT EDIT !!\n\n")
              (save-excursion
                ;; Replace autoload paths (only for module autoloads) with
                ;; absolute paths for faster resolution during load and
                ;; simpler `load-path'
                (let ((load-path (append doom-modules-dirs load-path))
                      cache)
                  (save-excursion
                    (while (re-search-forward "^\\s-*(autoload\\s-+'[^ ]+\\s-+\"\\([^\"]*\\)\"" nil t)
                      (let ((path (match-string 1)))
                        (replace-match
                         (or (cdr (assoc path cache))
                             (when-let* ((libpath (locate-library path))
                                         (libpath (file-name-sans-extension libpath)))
                               (push (cons path (abbreviate-file-name libpath)) cache)
                               libpath)
                             path)
                         t t nil 1)))
                    (print! (green "✓ Autoload paths expanded")))))
              ;; Remove byte-compile inhibiting file variables so we can
              ;; byte-compile the file.
              (when (re-search-forward "^;; no-byte-compile: t\n" nil t)
                (replace-match "" t t))
              ;; Byte compile it to give the file a chance to reveal errors.
              (save-buffer)
              (doom--byte-compile-file doom-autoload-file)
              (when (and noninteractive (not (daemonp)))
                (doom--server-load doom-autoload-file))
              t)
          (kill-buffer buf))))))

(defun doom//reload-package-autoloads (&optional force-p)
  "Compiles `doom-package-autoload-file' from the autoloads files of all
installed packages. It also caches `load-path', `Info-directory-list',
`doom-disabled-packages', `package-activated-list' and `auto-mode-alist'.

Will do nothing if none of your installed packages have been modified. If
FORCE-P (universal argument) is non-nil, regenerate it anyway.

This should be run whenever your `doom!' block or update your packages."
  (interactive)
  (if (and (not force-p)
           (not doom-emacs-changed-p)
           (file-exists-p doom-package-autoload-file)
           (not (file-newer-than-file-p package-user-dir doom-package-autoload-file))
           (not (ignore-errors
                  (cl-loop for key being the hash-keys of (doom-modules)
                           for path = (doom-module-path (car key) (cdr key) "packages.el")
                           if (file-newer-than-file-p path doom-package-autoload-file)
                           return t))))
      (ignore (print! (green "Doom package autoloads is up-to-date"))
              (doom-initialize-autoloads doom-package-autoload-file))
    (doom-delete-autoloads-file doom-package-autoload-file)
    (with-temp-file doom-package-autoload-file
      (insert ";;; -*- lexical-binding:t -*-\n"
              ";; This file is autogenerated by `doom//reload-package-autoloads', DO NOT EDIT !!\n\n")
      (let (case-fold-search)
        (save-excursion
          ;; Cache the important and expensive-to-initialize state here.
          (doom-initialize-packages)
          (prin1 `(setq load-path ',load-path
                        auto-mode-alist ',auto-mode-alist
                        Info-directory-list ',Info-directory-list
                        doom-disabled-packages ',doom-disabled-packages
                        package-activated-list ',package-activated-list)
                 (current-buffer))
          (print! (green "✓ Cached package state"))
          ;; insert package autoloads
          (dolist (spec package-alist)
            (if-let* ((pkg (car spec))
                      (desc (cadr spec)))
                (unless (memq pkg doom-autoload-excluded-packages)
                  (let ((file (concat (package--autoloads-file-name desc) ".el")))
                    (when (file-exists-p file)
                      (insert "(let ((load-file-name " (prin1-to-string (abbreviate-file-name file)) "))\n")
                      (insert-file-contents file)
                      (while (re-search-forward "^\\(?:;;\\(.*\n\\)\\|\n\\)" nil t)
                        (unless (nth 8 (syntax-ppss))
                          (replace-match "" t t)))
                      (unless (bolp) (insert "\n"))
                      (insert ")\n"))))
              (print! (yellow "⚠ Couldn't find package desc for %s" (car spec))))))
        (print! (green "✓ Package autoloads included"))
        ;; Remove `load-path' and `auto-mode-alist' modifications (most of them,
        ;; at least); they are cached later, so all those membership checks are
        ;; unnecessary overhead.
        (while (re-search-forward "^\\s-*\\((\\(?:add-to-list\\|\\(?:when\\|if\\) (boundp\\)\\s-+'\\(?:load-path\\|auto-mode-alist\\)\\)" nil t)
          (goto-char (match-beginning 1))
          (kill-sexp))
        (print! (green "✓ Removed load-path/auto-mode-alist entries"))))
    (doom--byte-compile-file doom-package-autoload-file)
    (when (and noninteractive (not (daemonp)))
      (doom--server-load doom-package-autoload-file))
    t))


;;
;; Byte compilation
;;

(defun doom//byte-compile (&optional modules recompile-p)
  "Byte compiles your emacs configuration.

init.el is always byte-compiled by this.

If MODULES is specified (a list of module strings, e.g. \"lang/php\"), those are
byte-compiled. Otherwise, all enabled modules are byte-compiled, including Doom
core. It always ignores unit tests and files with `no-byte-compile' enabled.

Doom was designed to benefit from byte-compilation, but the process may take a
while. Also, while your config files are byte-compiled, changes to them will not
take effect! Use `doom//clean-byte-compiled-files' or `make clean' to remove
these files.

If RECOMPILE-P is non-nil, only recompile out-of-date files."
  (interactive
   (list nil current-prefix-arg))
  (let ((default-directory doom-emacs-dir)
        (total-ok   0)
        (total-fail 0)
        (total-noop 0)
        compile-plugins-p
        targets)
    (dolist (module (delete-dups modules) (nreverse targets))
      (pcase module
        (":core"    (push doom-core-dir targets))
        (":private" (push doom-private-dir targets))
        (":plugins"
         (dolist (file (doom-files-in package-user-dir :match "\\.elc$"))
           (ignore-errors (delete-file file)))
         (byte-recompile-directory package-user-dir 0 t)
         (setq compile-plugins-p t
               modules (delete ":plugins" modules)))
        ((pred file-directory-p)
         (push module targets))
        ((pred (string-match "^\\([^/]+\\)/\\([^/]+\\)$"))
         (push (doom-module-locate-path
                (doom-keyword-intern (match-string 1 module))
                (intern (match-string 2 module)))
               targets))))
    (cl-block 'byte-compile
      ;; If we're just here to byte-compile our plugins, we're done!
      (and (not modules)
           compile-plugins-p
           (cl-return-from 'byte-compile t))
      (unless (or (equal modules '(":core"))
                  recompile-p)
        (unless (and (not doom-auto-accept)
                     (y-or-n-p
                      (concat "Warning: byte compiling is for advanced users. It will interfere with your\n"
                              "efforts to debug issues. It is not recommended you do it if you frequently\n"
                              "tinker with your Emacs config.\n\n"
                              "Alternatively, use `bin/doom compile :core` instead to byte-compile only the\n"
                              "Doom core files, as these don't change often.\n\n"
                              "If you have issues, please make sure byte-compilation isn't the cause by using\n"
                              "`bin/doom clean` to clear out your *.elc files.\n\n"
                              "Byte-compile anyway?")))
          (message "Aborting.")
          (cl-return-from 'byte-compile)))
      (unless recompile-p
        (doom//clean-byte-compiled-files)
        (doom//reload-autoloads))
      (let (doom-emacs-changed-p
            noninteractive)
        ;; But first we must be sure that Doom and your private config have
        ;; been fully loaded. Which usually aren't so in an noninteractive
        ;; session.
        (doom-initialize)
        (doom-initialize-modules 'force))
      ;; If no targets were supplied, then we use your module list.
      (unless modules
        (setq targets (append (list doom-core-dir)
                              (doom-module-load-path))))
      ;; Assemble el files we want to compile; taking into account that
      ;; MODULES may be a list of MODULE/SUBMODULE strings from the command
      ;; line.
      (let ((target-files (doom-files-in targets :depth 2 :match "\\.el$")))
        (unless target-files
          (if targets
              (message "Couldn't find any valid targets")
            (message "No targets to %scompile" (if recompile-p "re" "")))
          (cl-return-from 'byte-compile))
        (condition-case ex
            (let ((use-package-expand-minimally t))
              ;; Always compile private init file
              (push (expand-file-name "init.el" doom-private-dir) target-files)
              (push (expand-file-name "init.el" doom-emacs-dir)   target-files)
              (dolist (target (cl-delete-duplicates (mapcar #'file-truename target-files) :test #'equal))
                (if (or (not recompile-p)
                        (let ((elc-file (byte-compile-dest-file target)))
                          (and (file-exists-p elc-file)
                               (file-newer-than-file-p target elc-file))))
                    (let ((result (if (or (string-match-p "/\\(?:packages\\|doctor\\)\\.el$" target)
                                          (not (doom-file-cookie-p target)))
                                      'no-byte-compile
                                    (byte-compile-file target)))
                          (short-name (if (file-in-directory-p target doom-emacs-dir)
                                          (file-relative-name target doom-emacs-dir)
                                        (abbreviate-file-name target))))
                      (cl-incf
                       (cond ((eq result 'no-byte-compile)
                              (print! (dark (white "⚠ Ignored %s" short-name)))
                              total-noop)
                             ((null result)
                              (print! (red "✕ Failed to compile %s" short-name))
                              total-fail)
                             (t
                              (print! (green "✓ Compiled %s" short-name))
                              (quiet! (load target t t))
                              total-ok))))
                  (cl-incf total-noop)))
              (print!
               (bold
                (color (if (= total-fail 0) 'green 'red)
                       "%s %d/%d file(s) (%d ignored)"
                       (if recompile-p "Recompiled" "Compiled")
                       total-ok (- (length target-files) total-noop)
                       total-noop))))
          (error
           (print! (red "\n%%s\n\n%%s\n\n%%s")
                   "There were breaking errors."
                   (error-message-string ex)
                   "Reverting changes...")
           (quiet! (doom//clean-byte-compiled-files))
           (print! (yellow "Finished (nothing was byte-compiled)"))))))))

(defun doom//clean-byte-compiled-files ()
  "Delete all the compiled elc files in your Emacs configuration and private
module. This does not include your byte-compiled, third party packages.'"
  (interactive)
  (cl-loop with default-directory = doom-emacs-dir
           for path in (append (doom-files-in doom-emacs-dir :match "\\.elc$" :depth 1)
                               (doom-files-in doom-private-dir :match "\\.elc$" :depth 2)
                               (doom-files-in doom-core-dir :match "\\.elc$")
                               (doom-files-in doom-modules-dirs :match "\\.elc$" :depth 4))
           for truepath = (file-truename path)
           if (file-exists-p path)
           do (delete-file path)
           and do
           (print! (green "✓ Deleted %%s")
                   (if (file-in-directory-p truepath default-directory)
                       (file-relative-name truepath)
                     (abbreviate-file-name truepath)))
           finally do (print! (bold (green "Everything is clean")))))

(provide 'core-dispatcher)
;;; core-dispatcher.el ends here
