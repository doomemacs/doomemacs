;;; lisp/doom-profiles.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'doom)) ; be silent, o'byte-compiler


;;
;;; Variables

;;; File/directory variables
(defvar doom-profiles-generated-dir doom-data-dir
  "Where generated profiles are kept.

Profile directories are in the format {data-profiles-dir}/$NAME/@/$VERSION, for
example: '~/.local/share/doom/_/@/0/'")

(defvar doom-profile-load-path
  (if-let (path (getenv-internal "DOOMPROFILELOADPATH"))
      (mapcar #'expand-file-name (split-string-and-unquote path path-separator))
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
  (if-let (loader (getenv-internal "DOOMPROFILELOADFILE"))
      (expand-file-name loader doom-emacs-dir)
    (file-name-concat doom-emacs-dir (format "profiles/load.el" emacs-major-version)))
  "Where Doom writes its interactive profile loader script.

Can be changed externally by setting $DOOMPROFILELOADFILE.")

(defvar doom-profile-init-file-name (format "init.%d.el" emacs-major-version)
  "TODO")

(defvar doom-profile-init-dir-name (format "init.%d.d" emacs-major-version)
  "The subdirectory of `doom-profile-dir'")

(defvar doom-profiles-config-file-name ".doomprofile"
  "TODO")

;;; Profile storage variables
(defvar doom-profile-generators
  '(("05-init-vars.auto.el"         . doom-profile--generate-init-vars)
    ("80-loaddefs.auto.el"          . doom-profile--generate-doom-autoloads)
    ("90-loaddefs-packages.auto.el" . doom-profile--generate-package-autoloads)
    ("95-load-modules.auto.el"      . doom-profile--generate-load-modules))
  "An alist mapping file names to generator functions.

The file will be generated in `doom-profile-dir'/`doom-profile-init-dir-name',
and later combined into `doom-profile-dir'/`doom-profile-init-file-name' in
lexicographical order. These partials are left behind in case the use wants to
load them directly (for whatever use), or for commands to use (e.g.
`doom/reload-autoloads' loads any file with a NN-loaddefs[-.] prefix to
accomplish its namesake).

Files with an .auto.el suffix will be automatically deleted whenever the profile
is regenerated. Users (or Doom CLIs, like `doom env') may add their own
generators to this list, or to `doom-profile-dir'/`doom-profile-init-dir-name',
and they will be included in the profile init file next time `doom sync' is
run.")

(defvar doom--profiles ())

(defconst doom-profile-default (cons "_" "0"))


;;
;;; Helpers

(defun doom-profiles-bootloadable-p ()
  "Return non-nil if `doom-emacs-dir' can be a bootloader."
  (with-memoization (get 'doom 'bootloader)
    (or (file-equal-p doom-emacs-dir "~/.config/emacs")
        (file-equal-p doom-emacs-dir "~/.emacs.d"))))

(defun doom-profiles-read (&rest paths)
  "TODO"
  (let (profiles)
    (dolist (path (delq nil (flatten-list paths)))
      (cond
       ((file-directory-p path)
        (setq path (file-truename path))
        (dolist (subdir (doom-files-in path :depth 0 :match "/[^.][^/]+$" :type 'dirs :map #'file-name-base))
          (if (equal subdir (car doom-profile-default))
              (signal 'doom-profile-error (list (file-name-concat path subdir) "Implicit profile has invalid name"))
            (unless (string-prefix-p "_" subdir)
              (cl-pushnew
               (cons (intern subdir)
                     (let* ((val (abbreviate-file-name (file-name-as-directory subdir)))
                            (val (if (file-name-absolute-p val)
                                     `(,val)
                                   `(,(abbreviate-file-name path) ,val))))
                       (cons `(user-emacs-directory :path ,@val)
                             (if-let (profile-file (file-exists-p! doom-profiles-config-file-name path))
                                 (car (doom-file-read profile-file :by 'read*))
                               (when (file-exists-p (doom-path path subdir "lisp/doom.el"))
                                 '((doom-user-dir :path ,@val)))))))
               profiles
               :test #'eq
               :key #'car)))))
       ((file-exists-p path)
        (dolist (profile (car (doom-file-read path :by 'read*)))
          (if (eq (symbol-name (car profile)) (car doom-profile-default))
              (signal 'doom-profile-error (list path "Profile has invalid name: _"))
            (unless (string-prefix-p "_" (symbol-name (car profile)))
              (cl-pushnew profile profiles
                          :test #'eq
                          :key #'car)))))))
    (nreverse profiles)))

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

(defun doom-profile<-id (id)
  "Return a (NAME . VERSION) profile cons cell from an id string NAME@VERSION."
  (save-match-data
    (if (string-match "^\\([^@]+\\)@\\(.+\\)$" id)
        (cons (match-string 1 id)
              (match-string 2 id))
      (cons id (cdr doom-profile-default)))))

(defun doom-profile->id (profile)
  "Return a NAME@VERSION id string from profile cons cell (NAME . VERSION)."
  (cl-check-type profile cons)
  (format "%s@%s" (car profile) (cdr profile)))

;; TODO (defun doom-profile--read (profile)
;;   (doom-profile-create ))

;; TODO (defun doom-profile-initialize (profile-name &optional ref)
;;   )

(defun doom-profiles-save (profiles &optional file)
  "Generate a profile bootstrapper for Doom to load at startup."
  (unless file
    (setq file doom-profile-load-file))
  (doom-file-write
   file (let ((profilesym (make-symbol "profile"))
              (deferredsym (make-symbol "deferred-vars")))
          `(";; -*- lexical-binding: t; tab-width: 8; -*-\n"
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
            (setq user-emacs-directory (file-name-as-directory user-emacs-directory))))
   :mode #o600
   :printfn #'pp)
  (print-group!
    (or (let ((byte-compile-warnings (if init-file-debug byte-compile-warnings))
              (byte-compile-dest-file-function
               (lambda (_) (format "%s.%d.elc" (file-name-sans-extension file) emacs-major-version))))
          (byte-compile-file file))
        ;; Do it again? So the errors/warnings are visible?
        ;; (let ((byte-compile-warnings t))
        ;;   (byte-compile-file file))
        (signal 'doom-profile-error (list file "Failed to byte-compile bootstrap file")))))

(defun doom-profile-p (profile-name)
  "Return t if PROFILE-NAME is a valid and existing profile."
  (when (stringp profile-name)
    (setq profile-name (intern profile-name)))
  (and (assq profile-name (doom-profiles))
       t))

(defun doom-profile-get (profile-name &optional property null-value)
  "Return PROFILE-NAME's PROFILE, otherwise its PROPERTY, otherwise NULL-VALUE."
  (when (stringp profile-name)
    (setq profile-name (intern profile-name)))
  (if-let (profile (assq profile-name (doom-profiles)))
      (if property
          (if-let (propval (assq property (cdr profile)))
              (cdr propval)
            null-value)
        profile)
    null-value))

(defun doom-profile-emacs-dir (profile-name)
  "Return the `user-emacs-directory' for PROFILE-NAME.

If the profile doesn't specify one, fall back to `doom-emacs-dir'."
  (doom-profile-get profile-name 'user-emacs-directory doom-emacs-dir))

(defun doom-profile-init-file (&optional profile-id version)
  "Return the init file for PROFILE-ID at VERSION.

Defaults to the profile at `doom-profile-default'."
  (cl-destructuring-bind (profile . version)
      (if (and (stringp profile-id) (null version))
          (doom-profile<-id profile-id)
        (cl-check-type profile-id (or null string))
        (cl-check-type version (or null string))
        (cons (or profile-id ;; (car doom-profile-default)
                  )
              (or version    ;; (cdr doom-profile-default)
                  )))
    (file-name-concat doom-data-dir
                      profile "@" version
                      (format doom-profile-init-file-name emacs-major-version))))


;;
;;; Data structures

;; TODO


;;
;;; API

;; TODO (defun doom-profile-create (name))

;; TODO (defun doom-profile-hash (profile))

;; TODO (defmacro with-profile! (profile &rest body))


;;
;;; Generators

(defun doom-profile-generate (&optional _profile regenerate-only?)
  "Generate profile init files."
  (doom-initialize-packages)
  (let* ((default-directory doom-profile-dir)
         (init-dir  doom-profile-init-dir-name)
         (init-file doom-profile-init-file-name))
    (print! (start "(Re)building profile in %s/...") (dirname doom-profile-dir))
    (condition-case-unless-debug e
      (with-file-modes #o750
        (print-group!
          (make-directory init-dir t)
          (print! (start "Deleting old init files..."))
          (print-group! :level 'info
            (cl-loop for file in (cons init-file (doom-glob "*.elc"))
                     if (file-exists-p file)
                     do (print! (item "Deleting %s...") file)
                     and do (delete-file file)))
          (let ((auto-files (doom-glob init-dir "*.auto.el")))
            (print! (start "Generating %d init files...") (length doom-profile-generators))
            (print-group! :level 'info
              (dolist (file auto-files)
                (print! (item "Deleting %s...") file)
                (delete-file file))
              (pcase-dolist (`(,file . ,fn) doom-profile-generators)
                (let ((file (doom-path init-dir file)))
                  (doom-log "Building %s..." file)
                  (doom-file-write file (funcall fn))))))
          (with-file! init-file
            (insert ";; -*- coding: utf-8; lexical-binding: t; -*-\n"
                    ";; This file was autogenerated; do not edit it by hand!\n")
            ;; Doom needs to be synced/rebuilt if either Doom or Emacs has been
            ;; up/downgraded. This is because byte-code isn't backwards
            ;; compatible, and many packages (including Doom), bake in absolute
            ;; paths into their caches that need to be refreshed.
            (prin1 `(unless (equal doom-version ,doom-version)
                      (error ,(concat
                               "The installed version of Doom (%s) has changed (to %s) since last "
                               "'doom sync'. Run 'doom sync' to bring Doom up to speed")
                             ,doom-version doom-version))
                   (current-buffer))
            (dolist (file (doom-glob init-dir "*.el"))
              (print-group! :level 'info
                (print! (start "Reading %s...") file))
              (doom-file-read file :by 'insert)))
          (print! (start "Byte-compiling %s...") (relpath init-file))
          (print-group!
            (let ((byte-compile-warnings (if init-file-debug '(suspicious make-local callargs))))
              (byte-compile-file init-file)))
          (print! (success "Built %s") (byte-compile-dest-file init-file))))
      (error (delete-file init-file)
             (delete-file (byte-compile-dest-file init-file))
             (signal 'doom-autoload-error (list init-file e))))))

(defun doom-profile--generate-init-vars ()
  ;; FIX: Make sure this only runs at startup to protect us Emacs' interpreter
  ;;   re-evaluating this file when lazy-loading dynamic docstrings from the
  ;;   byte-compiled init file.
  `((when (doom-context-p 'init)
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
          (seq-filter (fn! (<= (doom-module-depth (car %) (cdr %) t) -100))
                      (remove '(:user) init-modules-list)))
         (init-modules
          (seq-filter (fn! (<= 0 (doom-module-depth (car %) (cdr %) t) 100))
                      init-modules-list))
         (config-modules
          (seq-filter (fn! (<= 0 (doom-module-depth (car %) (cdr %)) 100))
                      config-modules-list))
         (post-config-modules
          (seq-filter (fn! (>= (doom-module-depth (car %) (cdr %)) 100))
                      config-modules-list))
         (init-file   doom-module-init-file)
         (config-file doom-module-config-file))
    (letf! ((defun module-loader (group name file &optional noerror)
              (doom-module-context-with (cons group name)
                `(let ((doom-module-context ,doom-module-context))
                   (doom-load ,(abbreviate-file-name (file-name-sans-extension file))))))
            (defun module-list-loader (modules file &optional noerror)
              (cl-loop for (cat . mod) in modules
                       if (doom-module-locate-path cat mod file)
                       collect (module-loader cat mod it noerror))))
      ;; FIX: Same as above (see `doom-profile--generate-init-vars').
      `((if (or (doom-context-p 'init)
                (doom-context-p 'reload))
            (doom-context-with 'modules
              (set 'doom-modules ',doom-modules)
              (set 'doom-disabled-packages ',doom-disabled-packages)
              ;; Cache module state and flags in symbol plists for quick lookup by
              ;; `modulep!' later.
              ,@(cl-loop
                 for (category . modules) in (seq-group-by #'car config-modules-list)
                 collect
                 `(setplist ',category
                   (quote ,(cl-loop for (_ . module) in modules
                                    nconc `(,module ,(get category module))))))
              (let ((old-custom-file custom-file))
                ,@(module-list-loader pre-init-modules init-file)
                (doom-run-hooks 'doom-before-modules-init-hook)
                ,@(module-list-loader init-modules init-file)
                (doom-run-hooks 'doom-after-modules-init-hook)
                (doom-run-hooks 'doom-before-modules-config-hook)
                ,@(module-list-loader config-modules config-file)
                (doom-run-hooks 'doom-after-modules-config-hook)
                ,@(module-list-loader post-config-modules config-file t)
                (when (eq custom-file old-custom-file)
                  (doom-load custom-file 'noerror)))))))))

(defun doom-profile--generate-doom-autoloads ()
  (doom-autoloads--scan
   (append (doom-glob doom-core-dir "lib/*.el")
           (cl-loop for dir
                    in (append (doom-module-load-path doom-modules-dirs)
                               (list doom-user-dir))
                    if (doom-glob dir "autoload.el") collect (car it)
                    if (doom-glob dir "autoload/*.el") append it)
           (mapcan #'doom-glob doom-autoloads-files))
   nil))

(defun doom-profile--generate-package-autoloads ()
  (doom-autoloads--scan
   (mapcar #'straight--autoloads-file
           (nreverse (seq-difference (hash-table-keys straight--build-cache)
                                     doom-autoloads-excluded-packages)))
   doom-autoloads-excluded-files
   'literal))

(provide 'doom-profiles)
;;; doom-profiles.el ends here
