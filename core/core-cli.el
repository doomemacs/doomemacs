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
commands like `doom-packages-install', `doom-packages-update' and
`doom-packages-autoremove'.")

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
    (dolist (spec (cl-sort doom--dispatch-command-alist #'string-lessp
                           :key #'car))
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
              (assq (cdr (assq sym doom--dispatch-alias-alist))
                    doom--dispatch-command-alist)
              (error "Invalid command: %s" (car args))))
      (if help
          (apply #'doom--dispatch-help command desc (cdr args))
        (funcall body (cdr args))))))

(defmacro dispatcher! (command form &optional docstring)
  "Define a dispatcher command. COMMAND is a symbol or a list of symbols
representing the aliases for this command. DESC is a string description. The
first line should be short (under 60 letters), as it will be displayed for
bin/doom help.

BODY will be run when this dispatcher is called."
  (declare (doc-string 3))
  (cl-destructuring-bind (cmd &rest aliases)
      (doom-enlist command)
    (macroexp-progn
     (append
      (when aliases
        `((dolist (alias ',aliases)
            (setf (alist-get alias doom--dispatch-alias-alist) ',cmd))))
      `((setf (alist-get ',cmd doom--dispatch-command-alist)
              (list :desc ,docstring
                    :body (lambda (args) ,form))))))))


;;
;; Dispatch commands
;;

;; Dummy dispatchers (no-op because they're handled especially)
(dispatcher! run :noop
  "Run Doom Emacs from bin/doom's parent directory.

All arguments are passed on to Emacs (except for -p and -e).

  doom run
  doom run -nw init.el

WARNING: this command exists for convenience and testing. Doom will suffer
additional overhead for be started this way. For the best performance, it
is best to run Doom out of ~/.emacs.d and ~/.doom.d.")

(dispatcher! (doctor doc) :noop
  "Checks for issues with your current Doom config.")

(dispatcher! (help h) :noop
  "Look up additional information about a command.")


;; Real dispatchers
(dispatcher! (quickstart qs) (doom-quickstart)
  "Quickly deploy a private module and Doom.

This deploys a barebones config to ~/.doom.d. The destination can be changed
with the -p option, e.g.

  doom -p ~/.config/doom quickstart

This command will refuse to overwrite the private directory if it already
exists.")

(dispatcher! (install i) (doom--do 'doom-packages-install)
  "Installs requested plugins that aren't installed.")

(dispatcher! (update u) (doom--do 'doom-packages-update)
  "Installs requested plugins that aren't installed.")

(dispatcher! (autoremove r) (doom--do 'doom-packages-autoremove)
  "Installs requested plugins that aren't installed.")

(dispatcher! (autoloads a) (doom-reload-autoloads nil 'force)
  "Regenerates Doom's autoloads file.

This file tells Emacs where to find your module's autoloaded functions and
plugins.")


(dispatcher! (upgrade up) (doom-upgrade)
  "Checks out the latest Doom on this branch.")

(dispatcher! (compile c) (doom-byte-compile args)
  "Byte-compiles your config or selected modules.

  compile [TARGETS...]
  compile :core :private lang/python
  compile feature lang

Accepts :core, :private and :plugins as special arguments, indicating you want
to byte-compile Doom's core files, your private config or your ELPA plugins,
respectively.")

(dispatcher! (compile c) (doom-byte-compile args)
  "Byte-compiles your config or selected modules.

  compile [TARGETS...]
  compile :core :private lang/python
  compile feature lang

Accepts :core, :private and :plugins as special arguments, indicating you want
to byte-compile Doom's core files, your private config or your ELPA plugins,
respectively.")

(dispatcher! (recompile rc) (doom-byte-compile args 'recompile)
  "Re-byte-compiles outdated *.elc files.")

(dispatcher! clean (doom-clean-byte-compiled-files)
  "Delete all *.elc files.")


(dispatcher! test
  (progn (require 'core-tests)
         (doom-run-tests args))
  "Run Doom unit tests.")

(dispatcher! info (doom/info)
  "Output system info in markdown for bug reports.")

(dispatcher! (version v) (doom/version)
  "Reports the version of Doom and Emacs.")

(dispatcher! (refresh re) (doom-refresh 'force)
  "Refresh Doom. Same as autoremove+install+autoloads.

This is the equivalent of running autoremove, install, autoloads, then
recompile. Run this whenever you:

  1. Modify your `doom!' block,
  2. Add or remove `package!' blocks to your config,
  3. Add or remove autoloaded functions in module autoloaded files.
  4. Update Doom outside of Doom (e.g. with git)")


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

(defun doom-refresh (&optional force-p)
  "Ensure Doom is in a working state by checking autoloads and packages, and
recompiling any changed compiled files. This is the shotgun solution to most
problems with doom."
  (doom-reload-doom-autoloads force-p)
  (unwind-protect
      (progn
        (ignore-errors
          (doom-packages-autoremove doom-auto-accept))
        (ignore-errors
          (doom-packages-install doom-auto-accept)))
    (doom-reload-package-autoloads force-p)
    (doom-byte-compile nil 'recompile)))

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
           (message "There was an unexpected error.\n\n%s -> %s\n\nOutput:\n%s"
                    (car e)
                    (buffer-string))))))))

(defun doom-quickstart ()
  "Quickly deploy a private module and Doom.

This deploys a barebones config to `doom-private-dir', installs all missing
packages and regenerates the autoloads file."
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
  (doom-packages-install)
  (print! "Regenerating autoloads files")
  (doom-reload-autoloads nil 'force-p)
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
    (when-let* ((buf (find-buffer-visiting doom-autoload-file)))
      (with-current-buffer buf
        (set-buffer-modified-p nil))
      (kill-buffer buf))
    (delete-file file)
    (ignore-errors (delete-file (byte-compile-dest-file file)))
    (message "Deleted old %s" (file-name-nondirectory file))))

(defun doom--do (fn)
  (doom-reload-doom-autoloads)
  (when (funcall fn doom-auto-accept)
    (doom-reload-package-autoloads)))

(defun doom--warn-refresh-session ()
  (message "Detected a running Emacs session.\n")
  (message "Use `M-x doom/reload' for changes to take effect."))

(defun doom--do-load (&rest files)
  (if (and noninteractive (not (daemonp)))
      (progn
        (require 'server)
        (when (server-running-p)
          (add-hook 'kill-emacs-hook #'doom--warn-refresh-session)))
    (dolist (file files)
      (load-file (byte-compile-dest-file file)))))

(defun doom--byte-compile-file (file)
  (let ((short-name (file-name-nondirectory file))
        (byte-compile-dynamic-docstrings t))
    (condition-case e
        (when (byte-compile-file file)
          ;; Give autoloads file a chance to report error
          (load (if doom-debug-mode
                    file
                  (byte-compile-dest-file file))
                nil t)
          (unless noninteractive
            (message "Finished compiling %s" short-name)))
      ((debug error)
       (let ((backup-file (concat file ".bk")))
         (message "Copied backup to %s" backup-file)
         (copy-file file backup-file 'overwrite))
       (doom-delete-autoloads-file file)
       (signal 'doom-autoload-error (list short-name e))))))

(defun doom-reload-autoloads (&optional file force-p)
  "Reloads FILE (an autoload file), if it needs reloading.

FILE should be one of `doom-autoload-file' or `doom-package-autoload-file'. If
it is nil, it will try to reload both. If FORCE-P (universal argument) do it
even if it doesn't need reloading!"
  (or (null file)
      (stringp file)
      (signal 'wrong-type-argument (list 'stringp file)))
  (if (stringp file)
      (cond ((file-equal-p file doom-autoload-file)
             (doom-reload-doom-autoloads force-p))
            ((file-equal-p file doom-package-autoload-file)
             (doom-reload-package-autoloads force-p))
            ((error "Invalid autoloads file: %s" file)))
    (doom-reload-doom-autoloads force-p)
    (doom-reload-package-autoloads force-p)))


;;
;; Doom autoloads
;;

(defun doom--file-cookie-p (file)
  "Returns the return value of the ;;;###if predicate form in FILE."
  (with-temp-buffer
    (insert-file-contents-literally file nil 0 256)
    (if (and (re-search-forward "^;;;###if " nil t)
             (<= (line-number-at-pos) 3))
        (let ((load-file-name file))
          (eval (sexp-at-point)))
      t)))

(defun doom--generate-header (func)
  (goto-char (point-min))
  (insert ";; -*- lexical-binding:t -*-\n"
          ";; This file is autogenerated by `" (symbol-name func) "', DO NOT EDIT !!\n\n"))

(defun doom--generate-autoloads (targets)
  (require 'autoload)
  (dolist (file targets)
    (let* ((file (file-truename file))
           (generated-autoload-file doom-autoload-file)
           (generated-autoload-load-name (file-name-sans-extension file))
           (noninteractive (not doom-debug-mode))
           autoload-timestamps)
      (print!
       (cond ((not (doom--file-cookie-p file))
              "⚠ Ignoring %s")
             ((autoload-generate-file-autoloads file (current-buffer))
              (yellow "✕ Nothing in %%s"))
             ((green "✓ Scanned %%s")))
       (if (file-in-directory-p file default-directory)
           (file-relative-name file)
         (abbreviate-file-name file))))))

(defun doom--expand-autoloads ()
  (let ((load-path (append doom-modules-dirs load-path))
        cache)
    (while (re-search-forward "^\\s-*(autoload\\s-+'[^ ]+\\s-+\"\\([^\"]*\\)\"" nil t)
      (let ((path (match-string 1)))
        (replace-match
         (or (cdr (assoc path cache))
             (when-let* ((libpath (locate-library path))
                         (libpath (file-name-sans-extension libpath)))
               (push (cons path (abbreviate-file-name libpath)) cache)
               libpath)
             path)
         t t nil 1)))))

(defun doom--generate-autodefs (targets enabled-targets)
  (goto-char (point-max))
  (search-backward ";;;***" nil t)
  (save-excursion (insert "\n"))
  (dolist (path targets)
    (insert
     (with-temp-buffer
       (insert-file-contents path)
       (let ((member-p (or (member path enabled-targets)
                           (file-in-directory-p path doom-core-dir)))
             forms)
         (while (re-search-forward "^;;;###autodef *\\([^\n]+\\)?\n" nil t)
           (let* ((sexp (sexp-at-point))
                  (pred (match-string 1))
                  (type (car sexp))
                  (name (doom-unquote (cadr sexp)))
                  (origin (cond ((doom-module-from-path path))
                                ((file-in-directory-p path doom-private-dir)
                                 `(:private . ,(intern (file-name-base path))))
                                ((file-in-directory-p path doom-emacs-dir)
                                 `(:core . ,(intern (file-name-base path))))))
                  (doom-file-form
                   `(put ',name 'doom-file ,(abbreviate-file-name path))))
             (cond ((memq type '(defun defmacro cl-defun cl-defmacro))
                    (cl-destructuring-bind (type name arglist &rest body) sexp
                      (let ((docstring (if (stringp (car body))
                                           (pop body)
                                         "No documentation.")))
                        (push (cond ((not (and member-p
                                               (or (null pred)
                                                   (eval (read pred) t))))
                                     (push doom-file-form forms)
                                     (setq docstring (format "THIS FUNCTION DOES NOTHING BECAUSE %s IS DISABLED\n\n%s"
                                                             origin docstring))
                                     (condition-case-unless-debug e
                                         (append (list (pcase type
                                                         (`defun 'defmacro)
                                                         (`cl-defun `cl-defmacro))
                                                       name arglist docstring)
                                                 (cl-loop for arg in arglist
                                                          if (and (symbolp arg)
                                                                  (not (keywordp arg))
                                                                  (not (memq arg cl--lambda-list-keywords)))
                                                          collect arg into syms
                                                          else if (listp arg)
                                                          collect (car arg) into syms
                                                          finally return (if syms `((ignore ,@syms)))))
                                       ('error
                                        (message "Ignoring autodef %s (%s)"
                                                 name e)
                                        nil)))
                                    ((memq type '(defmacro cl-defmacro))
                                     (push doom-file-form forms)
                                     sexp)
                                    ((make-autoload sexp (abbreviate-file-name (file-name-sans-extension path)))))
                              forms)
                        (push `(put ',name 'doom-module ',origin) forms))))

                   ((eq type 'defalias)
                    (cl-destructuring-bind (type name target &optional docstring) sexp
                      (let ((name (doom-unquote name))
                            (target (doom-unquote target)))
                        (unless (and member-p
                                     (or (null pred)
                                         (eval (read pred) t)))
                          (setq target #'ignore))
                        (push doom-file-form forms)
                        (push `(put ',name 'doom-module ',origin) forms)
                        (push `(defalias ',name #',target ,docstring)
                              forms))))

                   ((and member-p
                         (or (null pred)
                             (eval (read pred) t)))
                    (push sexp forms)))))
         (if forms
             (concat (string-join (mapcar #'prin1-to-string (reverse forms)) "\n")
                     "\n")
           ""))))))

(defun doom--cleanup-autoloads ()
  (goto-char (point-min))
  (when (re-search-forward "^;;\\(;[^\n]*\\| no-byte-compile: t\\)\n" nil t)
    (replace-match "" t t)))

(defun doom-reload-doom-autoloads (&optional force-p)
  "Refreshes the autoloads.el file, specified by `doom-autoload-file', if
necessary (or if FORCE-P is non-nil).

It scans and reads core/autoload/*.el, modules/*/*/autoload.el and
modules/*/*/autoload/*.el, and generates `doom-autoload-file'. This file tells
Emacs where to find lazy-loaded functions.

This should be run whenever your `doom!' block, or a module autoload file, is
modified."
  (let* ((default-directory doom-emacs-dir)
         (doom-modules (doom-modules))
         (targets
          (file-expand-wildcards
           (expand-file-name "autoload/*.el" doom-core-dir)))
         (enabled-targets (copy-sequence targets))
         case-fold-search)
    (dolist (path (doom-module-load-path t))
      (let* ((auto-dir  (expand-file-name "autoload" path))
             (auto-file (expand-file-name "autoload.el" path))
             (module    (doom-module-from-path auto-file))
             (module-p  (or (doom-module-p (car module) (cdr module))
                            (file-equal-p path doom-private-dir))))
        (when (file-exists-p auto-file)
          (push auto-file targets)
          (if module-p (push auto-file enabled-targets)))
        (dolist (file (doom-files-in auto-dir :match "\\.el$" :full t))
          (push file targets)
          (if module-p (push file enabled-targets)))))
    (if (and (not force-p)
             (not doom-emacs-changed-p)
             (file-exists-p doom-autoload-file)
             (not (file-newer-than-file-p (expand-file-name "init.el" doom-private-dir)
                                          doom-autoload-file))
             (not (cl-loop for file in targets
                           if (file-newer-than-file-p file doom-autoload-file)
                           return t)))
        (progn (print! (green "Doom core autoloads is up-to-date"))
               (doom-initialize-autoloads doom-autoload-file)
               nil)
      (doom-delete-autoloads-file doom-autoload-file)
      (message "Generating new autoloads.el")
      (make-directory (file-name-directory doom-autoload-file) t)
      (with-temp-file doom-autoload-file
        (doom--generate-header 'doom-reload-doom-autoloads)
        (save-excursion
          (doom--generate-autoloads (reverse enabled-targets)))
          ;; Replace autoload paths (only for module autoloads) with absolute
          ;; paths for faster resolution during load and simpler `load-path'
        (save-excursion
          (doom--expand-autoloads)
          (print! (green "✓ Expanded module autoload paths")))
        ;; Generates stub definitions for functions/macros defined in disabled
        ;; modules, so that you will never get a void-function when you use
        ;; them.
        (save-excursion
          (doom--generate-autodefs (reverse targets) enabled-targets)
          (print! (green "✓ Generated autodefs")))
        ;; Remove byte-compile inhibiting file variables so we can byte-compile
        ;; the file, and autoload comments.
        (doom--cleanup-autoloads)
        (print! (green "✓ Clean up autoloads")))
      ;; Byte compile it to give the file a chance to reveal errors.
      (doom--byte-compile-file doom-autoload-file)
      (doom--do-load doom-autoload-file)
      t)))


;;
;; Package autoloads
;;

(defun doom--generate-package-autoloads ()
  (dolist (spec (doom-get-package-alist))
    (if-let* ((pkg  (car spec))
              (desc (cdr spec)))
        (unless (memq pkg doom-autoload-excluded-packages)
          (let ((file (concat (package--autoloads-file-name desc) ".el")))
            (when (file-exists-p file)
              (insert "(let ((load-file-name " (prin1-to-string (abbreviate-file-name file)) "))\n")
              (insert-file-contents file)
              (while (re-search-forward "^\\(?:;;\\(.*\n\\)\\|\n\\|(provide '[^\n]+\\)" nil t)
                (unless (nth 8 (syntax-ppss))
                  (replace-match "" t t)))
              (unless (bolp) (insert "\n"))
              (insert ")\n"))))
      (message "Couldn't find package desc for %s" (car spec)))))

(defun doom--generate-var-cache ()
  (doom-initialize-packages)
  (prin1 `(setq load-path ',load-path
                auto-mode-alist ',auto-mode-alist
                Info-directory-list ',Info-directory-list
                doom-disabled-packages ',doom-disabled-packages
                package-activated-list ',package-activated-list)
         (current-buffer)))

(defun doom--cleanup-package-autoloads ()
  (while (re-search-forward "^\\s-*\\((\\(?:add-to-list\\|\\(?:when\\|if\\) (boundp\\)\\s-+'\\(?:load-path\\|auto-mode-alist\\)\\)" nil t)
    (goto-char (match-beginning 1))
    (kill-sexp)))

(defun doom-reload-package-autoloads (&optional force-p)
  "Compiles `doom-package-autoload-file' from the autoloads files of all
installed packages. It also caches `load-path', `Info-directory-list',
`doom-disabled-packages', `package-activated-list' and `auto-mode-alist'.

Will do nothing if none of your installed packages have been modified. If
FORCE-P (universal argument) is non-nil, regenerate it anyway.

This should be run whenever your `doom!' block or update your packages."
  (if (and (not force-p)
           (not doom-emacs-changed-p)
           (file-exists-p doom-package-autoload-file)
           (not (file-newer-than-file-p doom-packages-dir doom-package-autoload-file))
           (not (ignore-errors
                  (cl-loop for key being the hash-keys of (doom-modules)
                           for path = (doom-module-path (car key) (cdr key) "packages.el")
                           if (file-newer-than-file-p path doom-package-autoload-file)
                           return t))))
      (ignore (print! (green "Doom package autoloads is up-to-date"))
              (doom-initialize-autoloads doom-package-autoload-file))
    (let (case-fold-search)
      (doom-delete-autoloads-file doom-package-autoload-file)
      (with-temp-file doom-package-autoload-file
        (doom--generate-header 'doom-reload-package-autoloads)
        (save-excursion
          ;; Cache the important and expensive-to-initialize state here.
          (doom--generate-var-cache)
          (print! (green "✓ Cached package state"))
          ;; Loop through packages and concatenate all their autoloads files.
          (doom--generate-package-autoloads)
          (print! (green "✓ Package autoloads included")))
        ;; Remove `load-path' and `auto-mode-alist' modifications (most of them,
        ;; at least); they are cached later, so all those membership checks are
        ;; unnecessary overhead.
        (doom--cleanup-package-autoloads)
        (print! (green "✓ Removed load-path/auto-mode-alist entries"))))
    (doom--byte-compile-file doom-package-autoload-file)
    (doom--do-load doom-package-autoload-file)
    t))


;;
;; Byte compilation
;;

(defun doom-byte-compile (&optional modules recompile-p)
  "Byte compiles your emacs configuration.

init.el is always byte-compiled by this.

If MODULES is specified (a list of module strings, e.g. \"lang/php\"), those are
byte-compiled. Otherwise, all enabled modules are byte-compiled, including Doom
core. It always ignores unit tests and files with `no-byte-compile' enabled.

WARNING: byte-compilation yields marginal gains and makes debugging new issues
difficult. It is recommended you don't use it unless you understand the
reprecussions.

Use `doom-clean-byte-compiled-files' or `make clean' to reverse
byte-compilation.

If RECOMPILE-P is non-nil, only recompile out-of-date files."
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
         (cl-loop for (name . desc) in (doom-get-package-alist)
                  do (package--compile desc))
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
      (and (not recompile-p)
           (or (null modules) (equal modules '(":core")))
           (doom-clean-byte-compiled-files))
      (let (doom-emacs-changed-p
            noninteractive)
        ;; But first we must be sure that Doom and your private config have been
        ;; fully loaded. Which usually aren't so in an noninteractive session.
        (unless (and (doom-initialize-autoloads doom-autoload-file)
                     (doom-initialize-autoloads doom-package-autoload-file))
          (doom-reload-autoloads))
        (doom-initialize)
        (doom-initialize-modules 'force))
      ;; If no targets were supplied, then we use your module list.
      (unless modules
        (setq targets (append (list doom-core-dir)
                              (doom-module-load-path))))
      ;; Assemble el files we want to compile; taking into account that
      ;; MODULES may be a list of MODULE/SUBMODULE strings from the command
      ;; line.
      (let ((target-files (doom-files-in targets :depth 1 :match "\\.el$"))
            (load-path load-path)
            kill-emacs-hook kill-buffer-query-functions)
        (unless target-files
          (if targets
              (message "Couldn't find any valid targets")
            (message "No targets to %scompile" (if recompile-p "re" "")))
          (cl-return-from 'byte-compile))
        (require 'use-package)
        (condition-case e
            (let ((use-package-defaults use-package-defaults)
                  (use-package-expand-minimally t))
              ;; Prevent packages from being loaded at compile time if they
              ;; don't meet their own predicates.
              (push (list :no-require t
                          (lambda (_name args)
                            (or (when-let* ((pred (or (plist-get args :if)
                                                      (plist-get args :when))))
                                  (not (eval pred t)))
                                (when-let* ((pred (plist-get args :unless)))
                                  (eval pred t)))))
                    use-package-defaults)
              ;; Always compile private init file
              (push (expand-file-name "init.el" doom-private-dir) target-files)
              (push (expand-file-name "init.el" doom-emacs-dir)   target-files)
              (dolist (target (cl-delete-duplicates (mapcar #'file-truename target-files) :test #'equal))
                (if (or (not recompile-p)
                        (let ((elc-file (byte-compile-dest-file target)))
                          (and (file-exists-p elc-file)
                               (file-newer-than-file-p target elc-file))))
                    (let ((result (if (or (string-match-p "/\\(?:packages\\|doctor\\)\\.el$" target)
                                          (not (doom--file-cookie-p target)))
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
          ((debug error)
           (print! (red "\n%s\n\n%%s" "There were breaking errors.")
                   "Reverting changes...")
           (signal 'doom-error (list 'byte-compile e))))))))

(defun doom-clean-byte-compiled-files ()
  "Delete all the compiled elc files in your Emacs configuration and private
module. This does not include your byte-compiled, third party packages.'"
  (cl-loop with default-directory = doom-emacs-dir
           for path in (append (doom-files-in doom-emacs-dir :match "\\.elc$" :depth 0)
                               (doom-files-in doom-private-dir :match "\\.elc$" :depth 1)
                               (doom-files-in doom-core-dir :match "\\.elc$")
                               (doom-files-in doom-modules-dirs :match "\\.elc$" :depth 3))
           for truepath = (file-truename path)
           if (file-exists-p path)
           do (delete-file path)
           and do
           (print! (green "✓ Deleted %%s")
                   (if (file-in-directory-p truepath default-directory)
                       (file-relative-name truepath)
                     (abbreviate-file-name truepath)))
           finally do (print! (bold (green "Everything is clean")))))

(provide 'core-cli)
;;; core-cli.el ends here
