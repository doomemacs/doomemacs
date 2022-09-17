;;; lisp/doom-profiles.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'doom)) ; be silent, o'byte-compiler


;;
;;; Variables

;;; File/directory variables
(defvar doom-profiles-dir doom-data-dir
  "Where generated profiles are kept.

Profile directories are in the format {data-profiles-dir}/$NAME/@/$VERSION, for
example: '~/.local/share/doom/_/@/0/'")

(defvar doom-profile-dirs
  (list (file-name-concat doom-user-dir "profiles")
        (file-name-concat doom-emacs-dir "profiles"))
  "A list of directories to search for implicit Doom profiles in.")

(defvar doom-profile-config-files
  (list (file-name-concat doom-user-dir "profiles.el")
        (file-name-concat doom-emacs-dir "profiles.el")
        (expand-file-name "doom-profiles.el" (or (getenv "XDG_CONFIG_HOME") "~/.config"))
        (expand-file-name "~/.doom-profiles.el"))
  "A list of potential locations for a profiles.el file.

`doom-profiles-initialize' will load and merge all profiles defined in the above
files, and will write a summary profiles.el to the first entry in this
variable.")

(defvar doom-profiles-bootstrap-file
  (file-name-concat doom-emacs-dir (format "profiles/init.%d.el" emacs-major-version))
  "Where Doom writes its profile bootstrap script.")

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

;; TODO Restore this in 3.0
(defconst doom-profile-default nil)
;; (defconst doom-profile-default (cons "_" "0"))


;;
;;; Helpers

(defun doom-profiles-read (&rest paths)
  "TODO"
  (let (profiles)
    (dolist (path (flatten-list paths))
      (cond
       ((file-directory-p path)
        (setq path (file-truename path))
        (dolist (subdir (doom-files-in path :depth 0 :match "/[^.][^/]+$" :type 'dirs :map #'file-name-base))
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
             :key #'car))))
       ((file-exists-p path)
        (dolist (profile (car (doom-file-read path :by 'read*)))
          (unless (string-prefix-p "_" (symbol-name (car profile)))
            (cl-pushnew profile profiles
                        :test #'eq
                        :key #'car))))))
    (when (assq '_ profiles)
      (signal 'doom-profile-error (list "Profile cannot be named _, as this is reserved for the implicit global profile")))
    (nreverse profiles)))

(defun doom-profiles-autodetect ()
  "Return all known profiles as a nested alist.

This reads all profiles in `doom-profile-config-files', then reads implicit profiles
living in `doom-profile-dirs', then caches them in `doom--profiles'. If RELOAD?
is non-nil, refresh the cache."
  (doom-profiles-read doom-profile-config-files
                      doom-profile-dirs))

(defun doom-profiles-outdated-p ()
  "Return non-nil if files in `doom-profiles-bootstrap-file' are outdated."
  (cl-find-if (doom-rpartial #'file-newer-than-file-p doom-profiles-bootstrap-file)
              doom-profile-config-files))

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

(defun doom-profiles-save (profiles file)
  "Generate a profile bootstrapper for Doom to load at startup."
  (doom-file-write
   file `(";; -*- lexical-binding: t; tab-width: 8; -*-\n"
          ";; Updated: " ,(format-time-string "%Y-%m-%d %H:%M:%S") "\n"
          ";; Generated by 'doom profiles sync' or 'doom sync'.\n"
          ";; DO NOT EDIT THIS BY HAND!\n"
          ,(format "%S" doom-version)
          (funcall
           (alist-get
            (intern (getenv-internal "DOOMPROFILE"))
            (list
             ,@(cl-loop
                with deferred?
                = (seq-find (fn! (memq (car-safe %) '(:prepend :prepend? :append :append?)))
                            (mapcar #'cdr profiles))
                with deferred-varsym = (make-symbol "deferred-vars")
                for (name . bindings) in profiles
                collect
                `(cons ',name
                  (lambda ()
                    (let ,(if deferred? '(--deferred-vars--))
                      ,@(cl-loop
                         for (var . val) in bindings
                         collect
                         (pcase (car-safe val)
                           (:path
                            `(,(if (stringp var) 'setenv 'set)
                              ',var ,(if (cddr val)
                                         (macroexpand-all
                                          `(cl-loop with path = ',(cadr val)
                                                    for dir in ',(cddr val)
                                                    do (setq path (expand-file-name dir path))
                                                    finally return path))
                                       `(file-truename ,(cadr val)))))
                           (:eval
                            (if (eq var '_)
                                (macroexp-progn (cdr val))
                              `(,(if (stringp var) 'setenv 'set)
                                ',var ,(macroexp-progn (cdr val)))))
                           (:plist
                            `(,(if (stringp var) 'setenv 'set)
                              ',var ',(if (stringp var)
                                          (prin1-to-string (cadr val))
                                        (cadr val))))
                           ((or :prepend :prepend?)
                            (if (stringp var)
                                `(setenv ',var (concat ,val (getenv ,var)))
                              (setq deferred? t)
                              `(push (cons ',var
                                           (lambda ()
                                             (dolist (item (list ,@(cdr val)))
                                               ,(if (eq (car val) :append?)
                                                    `(add-to-list ',var item)
                                                  `(push item ',var)))))
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
                                                  `(setq ',var (append ',var (list item)))))))
                                     --deferred-vars--)))
                           (_ `(,(if (stringp var) 'setenv 'set)
                                ',var ,(if (and (symbolp var)
                                                (or (eq var 'user-emacs-directory)
                                                    (string-match-p "^doom-.+-dir$" (symbol-name var))))
                                           `(file-truename ,var)
                                         ''var)))))
                      ,@(when deferred?
                          `((defun --defer-vars-- (_)
                              (dolist (var --deferred-vars--)
                                (when (boundp (car var))
                                  (funcall (cdr var))
                                  (setq --deferred-vars-- (delete var --deferred-vars--))))
                              (unless --deferred-vars--
                                (remove-hook 'after-load-functions #'--defer-vars--)
                                (unintern '--defer-vars-- obarray)
                                (unintern '--deferred-vars-- obarray)))
                            (add-hook 'after-load-functions #'--defer-vars--)
                            (--defer-vars--))))))))
            (lambda ()
              (if (or noninteractive
                      (file-equal-p user-emacs-directory "~/.config/emacs")
                      (file-equal-p user-emacs-directory "~/.emacs.d"))
                  (user-error "Failed to find profile: %s" (getenv "DOOMPROFILE"))
                (user-error "To be a bootloader, Doom must be installed in ~/.config/emacs or ~/.emacs.d"))))))
   :mode #o600
   :printfn #'pp)
  (byte-compile-file file))

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
        (cons (or profile-id (car doom-profile-default))
              (or version    (cdr doom-profile-default))))
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
            ;; compatible, and many packages (including Doom), make in absolute
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
  (setq doom-autoloads-cached-vars '(load-path
                                     Info-directory-list
                                     auto-mode-alist
                                     interpreter-mode-alist))
  (let ((v (version-to-list doom-version))
        (ref (doom-call-process "git" "-C" (doom-path doom-emacs-dir) "rev-parse" "HEAD"))
        (branch (doom-call-process "git" "-C" (doom-path doom-emacs-dir) "branch" "--show-current")))
    `(,@(cl-loop for var in doom-autoloads-cached-vars
                 if (boundp var)
                 collect `(set-default ',var ',(symbol-value var)))
      (setplist 'doom-version
                '(major  ,(nth 0 v)
                  minor  ,(nth 1 v)
                  build  ,(nth 2 v)
                  tag    ,(cadr (split-string doom-version "-" t))
                  ref    ,(if (zerop (car ref)) (cdr ref))
                  branch ,(if (zerop (car branch)) (cdr branch)))))))

(defun doom-profile--generate-load-modules ()
  (let ((module-list (cddr (doom-module-list))))
    `((set 'doom-disabled-packages ',doom-disabled-packages)
      (set 'doom-modules ',doom-modules)
      ;; Cache module state and flags in symbol plists for quick lookup by
      ;; `modulep!' later.
      ,@(cl-loop for (category . modules) in (seq-group-by #'car (doom-module-list))
                 collect `(setplist ',category
                           (quote ,(cl-loop for (_ . module) in modules
                                            nconc `(,module ,(get category module))))))
      (doom-run-hooks 'doom-before-modules-init-hook)
      ;; TODO: Until these files are byte-compiler-ready, I must use `load'
      ;;   instead of `require', as to not invite the byte-compiler to load them
      ;;   while this init file is compiled.
      (doom-load ,(doom-path doom-core-dir "doom-keybinds"))
      (doom-load ,(doom-path doom-core-dir "doom-ui"))
      (doom-load ,(doom-path doom-core-dir "doom-projects"))
      (doom-load ,(doom-path doom-core-dir "doom-editor"))
      ,@(cl-loop for (cat . mod) in module-list
                 if (doom-module-locate-path cat mod (concat doom-module-init-file ".el"))
                 collect `(let ((doom--current-module '(,cat . ,mod))
                                (doom--current-flags ',(doom-module-get cat mod :flags)))
                            (doom-load ,it)))
      (doom-run-hooks 'doom-after-modules-init-hook)
      (doom-run-hooks 'doom-before-modules-config-hook)
      ,@(cl-loop for (cat . mod) in module-list
                 if (doom-module-locate-path cat mod (concat doom-module-config-file ".el"))
                 collect `(let ((doom--current-module '(,cat . ,mod))
                                (doom--current-flags ',(doom-module-get cat mod :flags)))
                            (doom-load ,it)))
      (doom-run-hooks 'doom-after-modules-config-hook)
      (let ((old-custom-file custom-file))
        (doom-load ,(doom-path doom-user-dir doom-module-config-file) 'noerror)
        (when (eq custom-file old-custom-file)
          (doom-load custom-file 'noerror))))))

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
           (seq-difference (hash-table-keys straight--build-cache)
                           doom-autoloads-excluded-packages))
   doom-autoloads-excluded-files
   'literal))

(provide 'doom-profiles)
;;; doom-profiles.el ends here
