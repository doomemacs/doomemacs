;;; core/cli/byte-compile.el -*- lexical-binding: t; -*-

(defcli! (compile c) (&rest targets)
  "Byte-compiles your config or selected modules.

  compile [TARGETS...]
  compile :core :private lang/python
  compile feature lang

Accepts :core and :private as special arguments, which target Doom's core files
and your private config files, respectively. To recompile your packages, use
'doom rebuild' instead."
  (doom-byte-compile targets))

(defcli! (recompile rc) (&rest targets)
  "Re-byte-compiles outdated *.elc files."
  (doom-byte-compile targets 'recompile))

(defcli! clean ()
  "Delete all *.elc files."
  (doom-clean-byte-compiled-files))


;;
;; Helpers

(defun doom--byte-compile-ignore-file-p (path)
  (let ((filename (file-name-nondirectory path)))
    (or (string-prefix-p "." filename)
        (string-prefix-p "test-" filename)
        (not (equal (file-name-extension path) "el"))
        (member filename (list "packages.el" "doctor.el")))))

(cl-defun doom-byte-compile (&optional modules recompile-p)
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
        (doom-modules (doom-modules))
        (byte-compile-verbose doom-debug-mode)
        (byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

        ;; In case it is changed during compile-time
        (auto-mode-alist auto-mode-alist)
        (noninteractive t)

        targets)

    (let (target-dirs)
      (dolist (module (delete-dups modules))
        (pcase module
          (":core"
           (push (doom-glob doom-emacs-dir "init.el") targets)
           (push doom-core-dir target-dirs))
          (":private"
           (push doom-private-dir target-dirs))
          ((pred file-directory-p)
           (push module target-dirs))
          ((pred (string-match "^\\([^/]+\\)/\\([^/]+\\)$"))
           (push (doom-module-locate-path
                  (doom-keyword-intern (match-string 1 module))
                  (intern (match-string 2 module)))
                 target-dirs))))

      (and (or (null modules) (member ":private" modules))
           (not recompile-p)
           (not (or doom-auto-accept
                    (y-or-n-p
                     (concat "Warning: byte compiling is for advanced users. It will interfere with your\n"
                             "efforts to debug issues. It is not recommended you do it if you frequently\n"
                             "tinker with your Emacs config.\n\n"
                             "Alternatively, use `bin/doom compile :core` instead to byte-compile only the\n"
                             "Doom core files, as these don't change often.\n\n"
                             "If you have issues, please make sure byte-compilation isn't the cause by using\n"
                             "`bin/doom clean` to clear out your *.elc files.\n\n"
                             "Byte-compile anyway?"))))
           (user-error "Aborting"))

      ;; But first we must be sure that Doom and your private config have been
      ;; fully loaded. Which usually aren't so in an noninteractive session.
      (doom-load-autoloads-file doom-autoload-file)
      (doom-load-autoloads-file doom-package-autoload-file)

      ;;
      (unless target-dirs
        (push (doom-glob doom-emacs-dir "init.el") targets)
        ;; If no targets were supplied, then we use your module list.
        (appendq! target-dirs
                  (list doom-core-dir)
                  ;; Omit `doom-private-dir', which is always first
                  (cl-remove-if-not (lambda (path) (file-in-directory-p path doom-emacs-dir))
                                    (cdr (doom-module-load-path)))))

      ;; Assemble el files we want to compile; taking into account that MODULES
      ;; may be a list of MODULE/SUBMODULE strings from the command line.
      (appendq! targets
                (doom-files-in target-dirs
                               :match "\\.el$"
                               :filter #'doom--byte-compile-ignore-file-p)))

    (unless targets
      (print!
       (if targets
           (warn "Couldn't find any valid targets")
         (info "No targets to %scompile" (if recompile-p "re" ""))))
      (cl-return nil))

    (print!
     (info (if recompile-p
               "Recompiling stale elc files..."
             "Byte-compiling your config (may take a while)...")))
    (print-group!
     (require 'use-package)
     (condition-case e
         (let ((total-ok   0)
               (total-fail 0)
               (total-noop 0)
               (use-package-defaults use-package-defaults)
               (use-package-expand-minimally t)
               kill-emacs-hook kill-buffer-query-functions)
           ;; Prevent packages from being loaded at compile time if they
           ;; don't meet their own predicates.
           (push (list :no-require t
                       (lambda (_name args)
                         (or (when-let* ((pred (or (plist-get args :if))
                                                 (plist-get args :when)))
                               (not (eval pred t)))
                             (when-let* ((pred (plist-get args :unless)))
                               (eval pred t)))))
                 use-package-defaults)

           (unless recompile-p
             (doom-clean-byte-compiled-files))

           (dolist (target (delete-dups targets))
             (cl-incf
              (if (not (or (not recompile-p)
                           (let ((elc-file (byte-compile-dest-file target)))
                             (and (file-exists-p elc-file)
                                  (file-newer-than-file-p target elc-file)))))
                  total-noop
                (pcase (if (doom-file-cookie-p target)
                           (byte-compile-file target)
                         'no-byte-compile)
                  (`no-byte-compile
                   (print! (info "Ignored %s") (relpath target))
                   total-noop)
                  (`nil
                   (print! (error "Failed to compile %s") (relpath target))
                   total-fail)
                  (_
                   (print! (success "Compiled %s") (relpath target))
                   (load target t t)
                   total-ok)))))
           (print! (class (if (= total-fail 0) 'success 'error)
                          "%s %d/%d file(s) (%d ignored)")
                   (if recompile-p "Recompiled" "Compiled")
                   total-ok (- (length targets) total-noop)
                   total-noop)
           t)
       ((debug error)
        (print! (error "\nThere were breaking errors.\n\n%s")
                "Reverting changes...")
        (signal 'doom-error (list 'byte-compile e)))))))

(defun doom-clean-byte-compiled-files ()
  "Delete all the compiled elc files in your Emacs configuration and private
module. This does not include your byte-compiled, third party packages.'"
  (print! (start "Cleaning .elc files"))
  (print-group!
   (cl-loop with default-directory = doom-emacs-dir
            with success = nil
            for path
            in (append (doom-glob doom-emacs-dir "*.elc")
                       (doom-files-in doom-private-dir :match "\\.elc$" :depth 1)
                       (doom-files-in doom-core-dir :match "\\.elc$")
                       (doom-files-in doom-modules-dirs :match "\\.elc$" :depth 4))
            if (file-exists-p path)
            do (delete-file path)
            and do (print! (success "Deleted %s") (relpath path))
            and do (setq success t)
            finally do
            (print! (if success
                        (success "All elc files deleted")
                      (info "No elc files to clean"))))))
