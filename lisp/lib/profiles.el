;;; lisp/lib/profiles.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; File/directory variables
(defvar doom-profiles-generated-dir doom-data-dir
  "Where generated profiles are kept.

Profile directories are in the format {data-profiles-dir}/$NAME/@/$VERSION, for
example: '~/.local/share/doom/_/@/0/'")

(defvar doom-profile-load-path
  (append
   (when-let* ((path (getenv-internal "DOOMPROFILELOADPATH")))
     (mapcar #'doom-path (split-string-and-unquote path path-separator)))
   (list (file-name-concat doom-user-dir "profiles.el")
         (file-name-concat doom-emacs-dir "profiles.el")
         (expand-file-name "doom-profiles.el" (or (getenv "XDG_CONFIG_HOME") "~/.config"))
         (expand-file-name "~/.doom-profiles.el")
         (file-name-concat doom-user-dir "profiles")
         (file-name-concat doom-emacs-dir "profiles")))
  "A list of profile config files or directories that house implicit profiles.

`doom-profiles-initialize' loads and merges all profiles defined in the above
files/directories, then writes a profile load script to
`doom-profile-load-file'.

Can be changed externally by setting $DOOMPROFILELOADPATH to a colon-delimited
list of paths or profile config files (semi-colon delimited on Windows).")

(defvar doom-profile-load-file
  ;; REVIEW: Derive from `doom-data-dir' in v3
  (expand-file-name
   (format (or (getenv-internal "DOOMPROFILELOADFILE")
               (file-name-concat (if doom--system-windows-p "doomemacs/data" "doom")
                                 "profiles.%d.el"))
           emacs-major-version)
   (or (if doom--system-windows-p (getenv-internal "LOCALAPPDATA"))
       (getenv-internal "XDG_DATA_HOME")
       "~/.local/share"))
  "Where Doom writes its interactive profile loader script.

Can be changed externally by setting $DOOMPROFILELOADFILE.")

(defvar doom-profile-cache-file
  (file-name-concat
   doom-cache-dir (format "profiles.%s.el" (or (car doom-profile) "@")))
  "Where Doom writes its interactive profile loader script.

Can be changed externally by setting $DOOMPROFILELOADFILE.")

(defvar doom-profile-env-file-name "init.env.el"
  "The filename for a Doom profile's envvar file.")

(defvar doom-profile-init-dir-name (format "init.%d.d" emacs-major-version)
  "The subdirectory of `doom-profile-dir'")

(defvar doom-profile-rcfile ".doomprofile"
  "The filename for local user configuration of a Doom profile.")

;;; Profile storage variables
(defvar doom-profile-generators
  '(("05-vars.auto.el"              doom-profile--generate-vars               doom--startup-vars)
    ("80-loaddefs.auto.el"          doom-profile--generate-loaddefs-doom      doom--startup-loaddefs-doom)
    ("90-loaddefs-packages.auto.el" doom-profile--generate-loaddefs-packages  doom--startup-loaddefs-packages)
    ("95-modules.auto.el"           doom-profile--generate-load-modules       doom--startup-modules))
  "An alist mapping file names to generator functions.

The file will be generated in `doom-profile-dir'/`doom-profile-init-dir-name',
and later combined into `doom-profile-dir' in lexicographical order. These
partials are left behind in case the use wants to load them directly (for
whatever use), or for commands to use (e.g.  `doom/reload-autoloads' loads any
file with a NN-loaddefs[-.] prefix to accomplish its namesake).

Files with an .auto.el suffix will be automatically deleted whenever the profile
is regenerated. Users (or Doom CLIs, like `doom env') may add their own
generators to this list, or to `doom-profile-dir'/`doom-profile-init-dir-name',
and they will be included in the profile init file next time `doom sync' is
run.")

(defvar doom--profiles ())


;;
;;; Bootstrappers

;; (defun doom-profile-initialize (profile &optional project-dir nocache?))


;;
;;; Helpers

(defun doom-profiles-bootloadable-p ()
  "Return non-nil if `doom-emacs-dir' can be a bootloader.

This means it must be deployed to $XDG_CONFIG_HOME/emacs or $HOME/.emacs.d. Doom
cannot bootload from an arbitrary location."
  (with-memoization (get 'doom 'bootloader)
    (or (file-equal-p doom-emacs-dir "~/.emacs.d")
        (file-equal-p
         doom-emacs-dir (expand-file-name
                         "emacs/" (or (getenv "XDG_CONFIG_HOME")
                                      "~/.config"))))))

(defun doom-profiles-read (&rest paths)
  "TODO"
  (let ((key (doom-profile-key t))
        profiles)
    (dolist (path (delq nil (flatten-list paths)))
      (cond
       ((file-directory-p path)
        (setq path (file-truename path))
        (dolist (subdir (doom-files-in path :depth 0 :match "/[^.][^/]+$" :type 'dirs :map #'file-name-base))
          (if (equal subdir (car key))
              (signal 'doom-profile-error (list (file-name-concat path subdir) "Implicit profile has invalid name"))
            (unless (string-prefix-p "_" subdir)
              (cl-pushnew
               (cons (intern subdir)
                     (let* ((val (abbreviate-file-name (file-name-as-directory subdir)))
                            (val (if (file-name-absolute-p val)
                                     `(,val)
                                   `(,(abbreviate-file-name path) ,val))))
                       (cons `(user-emacs-directory :path ,@val)
                             (if-let* ((profile-file (file-exists-p! doom-profile-rcfile path)))
                                 (car (doom-file-read profile-file :by 'read*))
                               (when (file-exists-p (doom-path path subdir "lisp/doom.el"))
                                 '((doom-user-dir :path ,@val)))))))
               profiles
               :test #'eq
               :key #'car)))))
       ((file-exists-p path)
        (dolist (profile (car (doom-file-read path :by 'read*)))
          (if (eq (symbol-name (car profile)) (car key))
              (signal 'doom-profile-error (list path "Profile has invalid name: _"))
            (unless (string-prefix-p "_" (symbol-name (car profile)))
              (cl-pushnew profile profiles
                          :test #'eq
                          :key #'car)))))))
    (nreverse profiles)))

(defun doom-profiles-write-load-file (profiles &optional file)
  "Generate a profile bootstrapper for Doom to load at startup."
  (unless file
    (setq file doom-profile-load-file))
  (doom-file-write
   file `(";; -*- lexical-binding: t; tab-width: 8; -*-\n"
          ";; Updated: " ,(format-time-string "%Y-%m-%d %H:%M:%S") "\n"
          ";; Generated by 'doom profiles sync' or 'doom sync'.\n"
          ";; DO NOT EDIT THIS BY HAND!\n"
          ,(format "%S" doom-version)
          (pcase (intern (getenv-internal "DOOMPROFILE"))
            ,@(cl-loop
               for (profile-name . bindings) in profiles
               for deferred?
               = (seq-find (fn! (and (memq (car-safe (cdr %)) '(:prepend :prepend? :append :append?))
                                     (not (stringp (car-safe %)))))
                           bindings)
               collect
               `(',profile-name
                 (let ,(if deferred? '(--deferred-vars--))
                   ,@(cl-loop
                      for (var . val) in bindings
                      collect
                      (pcase (car-safe val)
                        (:path
                         `(,(if (stringp var) 'setenv 'setq)
                           ,var ,(cl-loop with form = `(expand-file-name ,(cadr val) user-emacs-directory)
                                          for dir in (cddr val)
                                          do (setq form `(expand-file-name ,dir ,form))
                                          finally return form)))
                        (:eval
                         (if (eq var '_)
                             (macroexp-progn (cdr val))
                           `(,(if (stringp var) 'setenv 'setq)
                             ,var ,(macroexp-progn (cdr val)))))
                        (:plist
                         `(,(if (stringp var) 'setenv 'setq)
                           ,var ',(if (stringp var)
                                      (prin1-to-string (cadr val))
                                    (cadr val))))
                        ((or :prepend :prepend?)
                         (if (stringp var)
                             `(setenv ,var (concat ,val (getenv ,var)))
                           (setq deferred? t)
                           `(push (cons ',var
                                        (lambda ()
                                          (dolist (item (list ,@(cdr val)))
                                            ,(if (eq (car val) :append?)
                                                 `(add-to-list ',var item)
                                               `(push item ,var)))))
                                  --deferred-vars--)))
                        ((or :append :append?)
                         (if (stringp var)
                             `(setenv ,var (concat (getenv ,var) ,val))
                           (setq deferred? t)
                           `(push (cons ',var
                                        (lambda ()
                                          (dolist (item (list ,@(cdr val)))
                                            ,(if (eq (car val) :append?)
                                                 `(add-to-list ',var item 'append)
                                               `(set ',var (append ,var (list item)))))))
                                  --deferred-vars--)))
                        (_ `(,(if (stringp var) 'setenv 'setq) ,var ',val))))
                   ,@(when deferred?
                       `((defun --doom-profile-set-deferred-vars-- (_)
                           (dolist (var --deferred-vars--)
                             (when (boundp (car var))
                               (funcall (cdr var))
                               (setq --deferred-vars-- (delete var --deferred-vars--))))
                           (unless --deferred-vars--
                             (remove-hook 'after-load-functions #'--doom-profile-set-deferred-vars--)
                             (unintern '--doom-profile-set-deferred-vars-- obarray)))
                         (add-hook 'after-load-functions #'--doom-profile-set-deferred-vars--)
                         (--doom-profile-set-deferred-vars-- nil)))))))
          ;; `user-emacs-directory' requires that it end in a directory
          ;; separator, but users may forget this in their profile configs.
          (setq user-emacs-directory (file-name-as-directory user-emacs-directory)))
   :mode (cons #o600 #o700)
   :printfn #'prin1)
  (print-group!
    (or (let ((byte-compile-debug t)
              (byte-compile-warnings (if init-file-debug byte-compile-warnings))
              (byte-compile-dest-file-function
               (lambda (_) (format "%s.elc" (file-name-sans-extension file)))))
          (byte-compile-file file))
        ;; Do it again? So the errors/warnings are visible?
        ;; (let ((byte-compile-warnings t))
        ;;   (byte-compile-file file))
        (signal 'doom-profile-error (list file "Failed to byte-compile bootstrap file")))))

(defun doom-profiles-autodetect (&optional _internal?)
  "Return all known profiles as a nested alist.

This reads all profile configs and directories in `doom-profile-load-path', then
caches them in `doom--profiles'. If RELOAD? is non-nil, refresh the cache."
  (doom-profiles-read doom-profile-load-path
                      ;; TODO: Add in v3
                      ;; (if internal? doom-profiles-generated-dir)
                      ))

(defun doom-profiles-outdated-p ()
  "Return non-nil if files in `doom-profile-load-file' are outdated."
  (cl-loop for path in doom-profile-load-path
           when (string-suffix-p path ".el")
           if (or (not (file-exists-p doom-profile-load-file))
                  (file-newer-than-file-p path doom-profile-load-file)
                  (not (equal (doom-file-read doom-profile-load-file :by 'read)
                              doom-version)))
           return t))


;;; Generators

(defun doom-profile-generate (&optional _profile regenerate-only?)
  "Generate profile init files."
  (doom-initialize-packages)
  (let* ((default-directory doom-profile-dir)
         (init-dir  doom-profile-init-dir-name)
         (init-file (doom-profile-init-file doom-profile)))
    (print! (start "(Re)building profile in %s/...") (path default-directory))
    (condition-case-unless-debug e
      (with-file-modes #o750
        (print-group!
          (make-directory init-dir t)
          (let ((auto-files (doom-glob init-dir "*.auto.el")))
            (print! (start "Generating %d init files...") (length doom-profile-generators))
            (print-group! :level 'info
              (dolist (file auto-files)
                (print! (item "Deleting %s...") file)
                (delete-file file))
              (pcase-dolist (`(,file ,fn _) doom-profile-generators)
                (let ((file (doom-path init-dir file)))
                  (doom-log "Building %s..." file)
                  (insert "\n;;;; START " file " ;;;;\n")
                  (doom-file-write file (funcall fn) :printfn #'prin1)
                  (insert "\n;;;; END " file " ;;;;\n")))))
          (with-file! init-file
            (insert ";; -*- coding: utf-8; lexical-binding: t; -*-\n"
                    ";; This file was autogenerated; do not edit it by hand!\n")
            ;; Doom needs to be synced/rebuilt if either Doom or Emacs has been
            ;; up/downgraded. This is because byte-code isn't backwards
            ;; compatible, and many packages (including Doom), bake in absolute
            ;; paths into their caches that need to be refreshed.
            (prin1 `(or (equal doom-version ,doom-version)
                        (error ,(concat
                                 "The installed version of Doom has changed since the last 'doom sync'.\n\n"
                                 "Run 'doom sync' to fix this.")
                               ,doom-version doom-version))
                   (current-buffer))
            (prin1 `(when (and (or initial-window-system
                                   (daemonp))
                               doom-env-file)
                      (doom-load-envvars-file doom-env-file 'noerror))
                   (current-buffer))
            (prin1 `(with-doom-context '(module init)
                      (doom-load (file-name-concat doom-user-dir ,doom-module-init-file) t))
                   (current-buffer))
            (dolist (file (doom-glob init-dir "*.el"))
              (print-group! :level 'info
                (print! (start "Reading %s...") file))
              (doom-file-read file :by 'insert))
            (prin1 `(defun doom-startup ()
                      ;; Make sure this only runs at startup to protect us
                      ;; Emacs' interpreter re-evaluating this file when
                      ;; lazy-loading dynamic docstrings from the byte-compiled
                      ;; init file.
                      (when (or (doom-context-p 'startup)
                                (doom-context-p 'reload))
                        ,@(cl-loop for (_ genfn initfn) in doom-profile-generators
                                   if (fboundp genfn)
                                   collect (list initfn))))
                   (current-buffer)))
          (print! (success "Built %s") (filename init-file))))
      (error (delete-file init-file)
             (signal 'doom-autoload-error (list init-file e))))))

(defun doom-profile--generate-vars ()
  `((defun doom--startup-vars ()
      ,@(cl-loop for var in doom-autoloads-cached-vars
                 if (boundp var)
                 collect `(set-default ',var ',(symbol-value var)))
      ,@(cl-loop with v = (version-to-list doom-version)
                 with ref = (doom-call-process "git" "-C" (doom-path doom-emacs-dir) "rev-parse" "HEAD")
                 with branch = (doom-call-process "git" "-C" (doom-path doom-emacs-dir) "branch" "--show-current")
                 for (var . val)
                 in `((major  . ,(nth 0 v))
                      (minor  . ,(nth 1 v))
                      (build  . ,(nth 2 v))
                      (tag    . ,(ignore-errors (cadr (split-string doom-version "-" t))))
                      (ref    . ,(if (zerop (car ref)) (cdr ref)))
                      (branch . ,(if (zerop (car branch)) (cdr branch))))
                 collect `(put 'doom-version ',var ',val)))))

(defun doom-profile--generate-load-modules ()
  (let* ((init-modules-list (doom-module-list nil t))
         (config-modules-list (doom-module-list))
         (pre-init-modules
          (seq-filter (fn! (<= (car (doom-module-get % :depth)) -100))
                      (remove '(:user . nil) init-modules-list)))
         (init-modules
          (seq-filter (fn! (<= 0 (car (doom-module-get % :depth)) 100))
                      init-modules-list))
         (config-modules
          (seq-filter (fn! (<= 0 (cdr (doom-module-get % :depth)) 100))
                      config-modules-list))
         (post-config-modules
          (seq-filter (fn! (>= (cdr (doom-module-get % :depth)) 100))
                      config-modules-list))
         (init-file   doom-module-init-file)
         (config-file doom-module-config-file))
    (letf! ((defun module-loader (key file)
              (let ((noextfile (file-name-sans-extension file)))
                `(with-doom-module ',key
                   ,(pcase key
                      ('(:doom . nil)
                       `(doom-load
                         (file-name-concat
                          doom-core-dir ,(file-name-nondirectory noextfile))
                         t))
                      ('(:user . nil)
                       `(doom-load
                         (file-name-concat
                          doom-user-dir ,(file-name-nondirectory noextfile))
                         t))
                      (_
                       (when (doom-file-cookie-p file "if" t)
                         `(doom-load ,(abbreviate-file-name noextfile) t)))))))
            (defun module-list-loader (modules file)
              (cl-loop for key in modules
                       if (doom-module-locate-path key file)
                       collect (module-loader key it))))
      ;; FIX: Same as above (see `doom-profile--generate-init-vars').
      `((set 'doom-modules ',doom-modules)
        (set 'doom-disabled-packages ',doom-disabled-packages)
        ;; Cache module state and flags in symbol plists for quick lookup by
        ;; `modulep!' later.
        ,@(cl-loop
           for (category . modules) in (seq-group-by #'car config-modules-list)
           collect
           `(setplist ',category
                      (quote ,(cl-loop for (_ . module) in modules
                                       nconc `(,module ,(doom-module->context (cons category module)))))))

        (defun doom--startup-modules ()
          (with-doom-context 'module
            (let ((old-custom-file custom-file))
              (with-doom-context 'init
                ,@(module-list-loader pre-init-modules init-file)
                (doom-run-hooks 'doom-before-modules-init-hook)
                ,@(module-list-loader init-modules init-file)
                (doom-run-hooks 'doom-after-modules-init-hook))
              (with-doom-context 'config
                (doom-run-hooks 'doom-before-modules-config-hook)
                ,@(module-list-loader config-modules config-file)
                (doom-run-hooks 'doom-after-modules-config-hook)
                ,@(module-list-loader post-config-modules config-file))
              (when (eq custom-file old-custom-file)
                (doom-load custom-file 'noerror)))))))))

(defun doom-profile--generate-loaddefs-doom ()
  `((defun doom--startup-loaddefs-doom ()
      (let ((load-in-progress t))
        ,@(doom-autoloads--scan
           (append (doom-glob doom-core-dir "lib/*.el")
                   (cl-loop for dir
                            in (append (doom-module-load-path)
                                       (list doom-user-dir))
                            if (doom-glob dir "autoload.el") collect (car it)
                            if (doom-glob dir "autoload/*.el") append it)
                   (mapcan #'doom-glob doom-autoloads-files))
           nil)))))

(defun doom-profile--generate-loaddefs-packages ()
  `((defun doom--startup-loaddefs-packages ()
      (let ((load-in-progress t))
        ,@(doom-autoloads--scan
           ;; Create a list of packages starting with the Nth-most dependencies
           ;; by walking the package dependency tree depth-first. This ensures
           ;; any load-order constraints in package autoloads are always met.
           (let (packages)
             (letf! (defun* walk-packages (pkglist)
                      (cond ((null pkglist) nil)
                            ((stringp pkglist)
                             (walk-packages (nth 1 (gethash pkglist straight--build-cache)))
                             (cl-pushnew pkglist packages :test #'equal))
                            ((listp pkglist)
                             (mapc #'walk-packages (reverse pkglist)))))
               (walk-packages (mapcar #'symbol-name (mapcar #'car doom-packages))))
             (mapcar #'straight--autoloads-file (nreverse packages)))
           doom-autoloads-excluded-files
           'literal))
      ,@(when-let* ((info-dirs
                     (cl-loop for key in (hash-table-keys straight--build-cache)
                              for dir = (straight--build-dir key)
                              for file = (straight--build-file dir "dir")
                              if (file-exists-p file)
                              collect dir)))
          `((require 'info)
            (info-initialize)
            (setq Info-directory-list
                  (append ',info-dirs Info-directory-list)))))))

(provide 'doom-lib '(profiles))
;;; profiles.el ends here
