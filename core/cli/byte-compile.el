;;; core/cli/byte-compile.el -*- lexical-binding: t; -*-

(defcli! (compile c)
    ((recompile-p ["-r" "--recompile"])
     (core-p      ["-c" "--core"])
     (private-p   ["-p" "--private"])
     (verbose-p   ["-v" "--verbose"]))
  "Byte-compiles your config or selected modules.

  compile [TARGETS...]
  compile :core :private lang/python
  compile feature lang

Accepts :core and :private as special arguments, which target Doom's core files
and your private config files, respectively. To recompile your packages, use
'doom build' instead."
  (doom-cli-byte-compile
   (if (or core-p private-p)
       (append (if core-p    (doom-glob doom-emacs-dir "init.el"))
               (if core-p    (list doom-core-dir))
               (if private-p (list doom-private-dir)))
     (or (y-or-n-p
          (concat "WARNING: Changes made to your config after compiling it won't take effect until\n"
                  "this command is rerun or you run 'doom clean'! It will also make error backtraces\n"
                  "much more difficult to decipher.\n\n"
                  "If you intend to use it anyway, remember this or it will come back to bite you!\n\n"
                  "Continue anyway?"))
         (user-error "Aborted"))
     (append (doom-glob doom-emacs-dir "init.el")
             (list doom-core-dir)
             (seq-filter
              ;; Only compile Doom's modules
              (doom-rpartial #'file-in-directory-p doom-emacs-dir)
              ;; Omit `doom-private-dir', which is always first
              (cdr (doom-module-load-path)))))
   recompile-p
   verbose-p))

(defcli! clean ()
  "Delete all *.elc files."
  :bare t
  (doom-clean-byte-compiled-files))


;;
;; Helpers

(defun doom--byte-compile-ignore-file-p (path)
  (let ((filename (file-name-nondirectory path)))
    (or (not (equal (file-name-extension path) "el"))
        (member filename (list "packages.el" "doctor.el"))
        (string-prefix-p "." filename)
        (string-prefix-p "test-" filename)
        (string-prefix-p "flycheck_" filename)
        (string-suffix-p ".example.el" filename))))

(cl-defun doom-cli-byte-compile (&optional targets recompile-p verbose-p)
  "Byte compiles your emacs configuration.

init.el is always byte-compiled by this.

If TARGETS is specified, as a list of direcotries

If MODULES is specified (a list of module strings, e.g. \"lang/php\"), those are
byte-compiled. Otherwise, all enabled modules are byte-compiled, including Doom
core. It always ignores unit tests and files with `no-byte-compile' enabled.

WARNING: byte-compilation yields marginal gains and makes debugging new issues
difficult. It is recommended you don't use it unless you understand the
reprecussions.

Use `doom-clean-byte-compiled-files' or `make clean' to reverse
byte-compilation.

If RECOMPILE-P is non-nil, only recompile out-of-date files."
  (let* ((default-directory doom-emacs-dir)
         (targets (nreverse (delete-dups targets)))
         ;; In case it is changed during compile-time
         (auto-mode-alist auto-mode-alist)
         kill-emacs-hook kill-buffer-query-functions)

    (let ((after-load-functions
           (if (null targets)
               after-load-functions
             ;; Assemble el files we want to compile, and preserve in the order
             ;; they are loaded in, so we don't run into any scary catch-22s
             ;; while byte-compiling, like missing macros.
             (cons (let ((target-dirs (cl-remove-if-not #'file-directory-p targets)))
                     (lambda (path)
                       (and (not (doom--byte-compile-ignore-file-p path))
                            (cl-find-if (doom-partial #'file-in-directory-p path)
                                        target-dirs)
                            (cl-pushnew path targets))))
                   after-load-functions))))
      (doom-log "Reloading Doom in preparation for byte-compilation")
      ;; But first we must be sure that Doom and your private config have been
      ;; fully loaded. Which usually aren't so in an noninteractive session.
      (let ((load-prefer-newer t)
            (noninteractive t)
            doom-interactive-p)
        (doom-initialize 'force)
        (quiet! (doom-initialize-packages))
        (quiet! (doom-initialize-modules))))

    (if (null targets)
        (print! (info "No targets to %scompile" (if recompile-p "re" "")))
      (print! (start "%scompiling your config...")
              (if recompile-p "Re" "Byte-"))

      (dolist (dir
               (cl-remove-if-not #'file-directory-p targets)
               (setq targets (cl-remove-if #'file-directory-p targets)))
        (prependq! targets
                   (doom-files-in
                    dir :match "\\.el" :filter #'doom--byte-compile-ignore-file-p)))

      (print-group!
       (require 'use-package)
       (condition-case-unless-debug e
           (let* ((total-ok   0)
                  (total-fail 0)
                  (total-noop 0)
                  (byte-compile-verbose nil)
                  (byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
                  (byte-compile-dynamic-docstrings t)
                  (use-package-compute-statistics nil)
                  (use-package-defaults use-package-defaults)
                  (use-package-expand-minimally t)
                  (targets (delete-dups targets))
                  (modules (seq-group-by #'doom-module-from-path targets))
                  (total-files (length targets))
                  (total-modules (length modules))
                  (i 0)
                  last-module)
             ;; Prevent packages from being loaded at compile time if they
             ;; don't meet their own predicates.
             (push (list :no-require t
                         (lambda (_name args)
                           (or (when-let (pred (or (plist-get args :if)
                                                   (plist-get args :when)))
                                 (not (eval pred t)))
                               (when-let (pred (plist-get args :unless))
                                 (eval pred t)))))
                   use-package-defaults)
             (dolist (module-files modules)
               (cl-incf i)
               (dolist (target (cdr module-files))
                 (let ((elc-file (byte-compile-dest-file target)))
                   (cl-incf
                    (if (and recompile-p (not (file-newer-than-file-p target elc-file)))
                        total-noop
                      (pcase (if (not (doom-file-cookie-p target "if" t))
                                 'no-byte-compile
                               (unless (equal last-module (car module-files))
                                 (print! (success "(% 3d/%d) Compiling %s")
                                         i total-modules
                                         (if-let (m (caar module-files))
                                             (format "%s %s module..." m (cdar module-files))
                                           (format "%d stand alone elisp files..."
                                                   (length (cdr module-files))))
                                         (caar module-files) (cdar module-files))
                                 (setq last-module (car module-files)))
                               (if verbose-p
                                   (byte-compile-file target)
                                 (quiet! (byte-compile-file target))))
                        (`no-byte-compile
                         (print! (debug "(% 3d/%d) Ignored %s")
                                 i total-modules (relpath target))
                         total-noop)
                        (`nil
                         (print! (error "(% 3d/%d) Failed to compile %s")
                                 i total-modules (relpath target))
                         total-fail)
                        (_ total-ok)))))))
             (print! (class (if (= total-fail 0) 'success 'warn)
                            "%s %d/%d file(s) (%d ignored)")
                     (if recompile-p "Recompiled" "Byte-compiled")
                     total-ok total-files
                     total-noop)
             (= total-fail 0))
         ((debug error)
          (print! (error "There were breaking errors.\n\n%s")
                  "Reverting changes...")
          (signal 'doom-error (list 'byte-compile e))))))))

(defun doom-clean-byte-compiled-files ()
  "Delete all the compiled elc files in your Emacs configuration and private
module. This does not include your byte-compiled, third party packages.'"
  (require 'core-modules)
  (print! (start "Cleaning .elc files"))
  (print-group!
   (cl-loop with default-directory = doom-emacs-dir
            with success = 0
            with esc = (if doom-debug-p "" "\033[1A")
            for path
            in (append (doom-glob doom-emacs-dir "*.elc")
                       (doom-files-in doom-private-dir :match "\\.elc$" :depth 1)
                       (doom-files-in doom-core-dir :match "\\.elc$")
                       (doom-files-in doom-modules-dirs :match "\\.elc$" :depth 4))
            if (file-exists-p path)
            do (delete-file path)
            and do (print! (success "\033[KDeleted %s%s") (relpath path) esc)
            and do (cl-incf success)
            finally do
            (print! (if (> success 0)
                        (success "\033[K%d elc files deleted" success)
                      (info "\033[KNo elc files to clean"))))
   t))
