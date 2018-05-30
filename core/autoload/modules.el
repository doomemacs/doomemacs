;;; core/autoload/modules.el -*- lexical-binding: t; -*-

(autoload 'print! "autoload/message" nil 'macro)

(defun doom--server-load (file)
  (require 'server)
  (when (server-running-p)
    (server-eval-at server-name `(load-file ,(byte-compile-dest-file file)))))

;;;###autoload
(defun doom//reload (&optional force-p)
  "Reloads your config. This is experimental!

If called from a noninteractive session, this will try to communicate with a
live server (if one is found) to tell it to run this function.

If called from an interactive session, tries to reload autoloads files (if
necessary), reinistalize doom (via `doom-initialize') and reloads your private
init.el and config.el. Then runs `doom-reload-hook'."
  (interactive)
  (cond ((and noninteractive (not (daemonp)))
         (require 'server)
         (if (not (server-running-p))
             (doom//reload-autoloads force-p)
           (print! "Reloading active Emacs session...")
           (print!
            (bold "%%s")
            (if (server-eval-at server-name '(doom//reload))
                (green "Done!")
              (red "There were issues!")))))
        ((let ((load-prefer-newer t))
           (doom//reload-autoloads force-p)
           (doom-initialize t)
           (ignore-errors (doom-initialize-modules t))
           (print! (green "%d packages reloaded" (length package-alist)))
           (run-hooks 'doom-reload-hook)
           t))))


;;
;; Autoload file generation
;;

(defvar doom-autoload-excluded-packages '(marshal gh)
  "Packages that have silly or destructive autoload files that try to load
everyone in the universe and their dog, causing errors that make babies cry. No
one wants that.")

(defun doom--byte-compile (file)
  (let ((short-name (file-name-nondirectory file)))
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

;;;###autoload
(defun doom-delete-autoloads-file (file)
  "Delete FILE (an autoloads file), and delete the accompanying *.elc file, if
it exists."
  (or (stringp file)
      (signal 'wrong-type-argument (list 'stringp file)))
  (when (file-exists-p file)
    (delete-file file)
    (ignore-errors (delete-file (byte-compile-dest-file file)))
    (print! "Deleted old %s" (file-name-nondirectory file))))

;;;###autoload
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
;;;###autoload
(defun doom//reload-doom-autoloads (&optional force-p)
  "Refreshes the autoloads.el file, specified by `doom-autoload-file', if
necessary (or if FORCE-P is non-nil).

It scans and reads core/autoload/*.el, modules/*/*/autoload.el and
modules/*/*/autoload/*.el, and generates `doom-autoload-file'. This file tells
Emacs where to find lazy-loaded functions.

This should be run whenever your `doom!' block, or a module autoload file, is
modified."
  (interactive)
  (let ((doom-modules (doom-module-table))
        (default-directory doom-emacs-dir)
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
                (let ((load-path (append doom-psuedo-module-dirs
                                         doom-modules-dirs
                                         load-path))
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
              (doom--byte-compile doom-autoload-file)
              (when (and noninteractive (not (daemonp)))
                (doom--server-load doom-autoload-file))
              t)
          (kill-buffer buf))))))

;;;###autoload
(defun doom//reload-package-autoloads (&optional force-p)
  "Compiles `doom-package-autoload-file' from the autoloads files of all
installed packages. It also caches `load-path', `Info-directory-list',
`doom-disabled-packages', `package-activated-list' and `auto-mode-alist'.

Will do nothing if none of your installed packages have been modified. If
FORCE-P (universal argument) is non-nil, regenerate it anyway.

This should be run whenever your `doom!' block or update your packages."
  (interactive)
  (if (and (not force-p)
           (file-exists-p doom-package-autoload-file)
           (not (file-newer-than-file-p package-user-dir doom-package-autoload-file)))
      (ignore (print! (green "Doom package autoloads is up-to-date"))
              (doom-initialize-autoloads doom-package-autoload-file))
    (doom-delete-autoloads-file doom-package-autoload-file)
    (with-temp-file doom-package-autoload-file
      (insert ";;; -*- lexical-binding:t -*-\n"
              ";; This file is autogenerated by `doom//reload-package-autoloads', DO NOT EDIT !!\n\n")
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
      (print! (green "✓ Removed load-path/auto-mode-alist entries")))
    (doom--byte-compile doom-package-autoload-file)
    (when (and noninteractive (not (daemonp)))
      (doom--server-load doom-package-autoload-file))
    t))


;;
;; Byte compilation
;;

;;;###autoload
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
  (let ((default-directory doom-emacs-dir))
    (unless recompile-p
      (doom//clean-byte-compiled-files))
    (let ((total-ok   0)
          (total-fail 0)
          (total-noop 0)
          compile-plugins-p
          targets)
      (dolist (module (delete-dups modules) (nreverse targets))
        (pcase module
          (":core"    (push doom-core-dir targets))
          (":private" (push doom-private-dir targets))
          (":plugins"
           (byte-recompile-directory package-user-dir 0 t)
           (setq compile-plugins-p t))
          ((pred file-directory-p)
           (push module targets))
          ((pred (string-match "^\\([^/]+\\)/\\([^/]+\\)$"))
           (push (doom-module-locate-path
                  (intern (format ":%s" (match-string 1 module)))
                  (intern (match-string 2 module)))
                 targets))))
      (unless (equal modules (list ":plugins"))
        (let ((inhibit-message t)
              noninteractive)
          ;; But first we must be sure that Doom and your private config have been
          ;; fully loaded. Which usually aren't so in an noninteractive session.
          (doom//reload-autoloads)
          (doom-initialize t)))
      ;; If no targets were supplied, then we use your module list.
      (unless targets
        (doom-initialize-modules t)
        (setq targets (append (list doom-core-dir)
                              (doom-module-load-path))))
      ;; Assemble el files we want to compile; taking into account that MODULES
      ;; may be a list of MODULE/SUBMODULE strings from the command line.
      (let ((target-files (doom-files-in targets :depth 2 :match "\\.el$")))
        (if (not target-files)
            (unless compile-plugins-p
              (message "No targets to %scompile" (if recompile-p "re" "")))
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
             (print! (yellow "Finished (nothing was byte-compiled)")))))))))

;;;###autoload
(defun doom//clean-byte-compiled-files ()
  "Delete all the compiled elc files in your Emacs configuration and private
module. This does not include your byte-compiled, third party packages.'"
  (interactive)
  (cl-loop with default-directory = doom-emacs-dir
           for path in (append (doom-files-in doom-emacs-dir :match "\\.elc$" :depth 1)
                               (doom-files-in doom-psuedo-module-dirs :match "\\.elc$" :depth 1)
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
