;;; core.el --- the heart of the beast -*- lexical-binding: t; -*-

(eval-when-compile
  (and (version< emacs-version "25")
       (error "Detected Emacs %s. Doom only supports Emacs 25.1 and higher"
              emacs-version)))

(defvar doom-debug-mode (or (getenv "DEBUG") init-file-debug)
  "If non-nil, all doom functions will be verbose. Set DEBUG=1 in the command
line or use --debug-init to enable this.")


;;
;; Constants

(defconst doom-version "2.0.9"
  "Current version of DOOM emacs.")

(defconst EMACS26+ (> emacs-major-version 25))
(defconst EMACS27+ (> emacs-major-version 26))

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))


;;
(defvar doom-emacs-dir
  (eval-when-compile (file-truename user-emacs-directory))
  "The path to this emacs.d directory. Must end in a slash.")

(defvar doom-core-dir (concat doom-emacs-dir "core/")
  "The root directory of core Doom files.")

(defvar doom-modules-dir (concat doom-emacs-dir "modules/")
  "The root directory for Doom's modules.")

(defvar doom-local-dir (concat doom-emacs-dir ".local/")
  "Root directory for local Emacs files. Use this as permanent storage for files
that are safe to share across systems (if this config is symlinked across
several computers).")

(defvar doom-etc-dir (concat doom-local-dir "etc/")
  "Directory for non-volatile storage.

Use this for files that don't change much, like servers binaries, external
dependencies or long-term shared data.")

(defvar doom-cache-dir (concat doom-local-dir "cache/")
  "Directory for volatile storage.

Use this for files that change often, like cache files.")

(defvar doom-packages-dir (concat doom-local-dir "packages/")
  "Where package.el and quelpa plugins (and their caches) are stored.")

(defvar doom-docs-dir (concat doom-emacs-dir "docs/")
  "Where the Doom manual is stored.")

(defvar doom-private-dir
  (or (getenv "DOOMDIR")
      (let ((xdg-path
             (expand-file-name "doom/"
                               (or (getenv "XDG_CONFIG_HOME")
                                   "~/.config"))))
        (if (file-directory-p xdg-path) xdg-path))
      "~/.doom.d/")
  "Where your private customizations are placed. Must end in a slash. Respects
XDG directory conventions if ~/.config/doom exists.")

(defvar doom-autoload-file (concat doom-local-dir "autoloads.el")
  "Where `doom-reload-doom-autoloads' will generate its core autoloads file.")

(defvar doom-package-autoload-file (concat doom-local-dir "autoloads.pkg.el")
  "Where `doom-reload-package-autoloads' will generate its package.el autoloads
file.")


;;
;; Doom core variables

(defvar doom-init-p nil
  "Non-nil if `doom-initialize' has run.")

(defvar doom-init-time nil
  "The time it took, in seconds, for DOOM Emacs to initialize.")

(defvar doom-emacs-changed-p nil
  "If non-nil, the running version of Emacs is different from the first time
Doom was setup, which can cause problems.")

(defvar doom-site-load-path load-path
  "The starting load-path, before it is altered by `doom-initialize'.")

(defvar doom--last-emacs-file (concat doom-local-dir "emacs-version.el"))
(defvar doom--last-emacs-version nil)
(defvar doom--refreshed-p nil)
(defvar doom--stage 'init)


;;
;; Custom error types

(define-error 'doom-error "Error in Doom Emacs core")
(define-error 'doom-hook-error "Error in a Doom startup hook" 'doom-error)
(define-error 'doom-autoload-error "Error in an autoloads file" 'doom-error)
(define-error 'doom-module-error "Error in a Doom module" 'doom-error)
(define-error 'doom-private-error "Error in private config" 'doom-error)
(define-error 'doom-package-error "Error with packages" 'doom-error)


;;
;; Custom hooks

(defvar doom-init-hook nil
  "Hooks run after all init.el files are loaded, including your private and all
module init.el files, but before their config.el files are loaded.")

(defvar doom-post-init-hook nil
  "A list of hooks run when Doom is fully initialized. Fires near the end of
`emacs-startup-hook', as late as possible. Guaranteed to run after everything
else (except for `window-setup-hook').")

(defvar doom-reload-hook nil
  "A list of hooks to run when `doom/reload' is called.")

(defvar doom-load-theme-hook nil
  "Hook run after the theme is loaded with `load-theme' or reloaded with
`doom/reload-theme'.")

(defvar doom-exit-window-hook nil
  "Hook run before `switch-window' or `switch-frame' are called.

Also see `doom-enter-window-hook'.")

(defvar doom-enter-window-hook nil
  "Hook run after `switch-window' or `switch-frame' are called.

Also see `doom-exit-window-hook'.")

(defvar doom-exit-buffer-hook nil
  "Hook run after `switch-to-buffer', `pop-to-buffer' or `display-buffer' are
called. The buffer to be switched to is current when these hooks run.

Also see `doom-enter-buffer-hook'.")

(defvar doom-enter-buffer-hook nil
  "Hook run before `switch-to-buffer', `pop-to-buffer' or `display-buffer' are
called. The buffer to be switched to is current when these hooks run.

Also see `doom-exit-buffer-hook'.")

(defvar doom-inhibit-switch-buffer-hooks nil
  "Letvar for inhibiting `doom-enter-buffer-hook' and `doom-exit-buffer-hook'.
Do not set this directly.")
(defvar doom-inhibit-switch-window-hooks nil
  "Letvar for inhibiting `doom-enter-window-hook' and `doom-exit-window-hook'.
Do not set this directly.")

(defun doom*switch-window-hooks (orig-fn window &optional norecord)
  (if (or doom-inhibit-switch-window-hooks
          (null window)
          (eq window (selected-window))
          (window-minibuffer-p)
          (window-minibuffer-p window))
      (funcall orig-fn window norecord)
    (let ((doom-inhibit-switch-window-hooks t))
      (run-hooks 'doom-exit-window-hook)
      (prog1 (funcall orig-fn window norecord)
        (with-selected-window window
          (run-hooks 'doom-enter-window-hook))))))

(defun doom*switch-buffer-hooks (orig-fn buffer-or-name &rest args)
  (if (or doom-inhibit-switch-buffer-hooks
          (eq (get-buffer buffer-or-name) (current-buffer)))
      (apply orig-fn buffer-or-name args)
    (let ((doom-inhibit-switch-buffer-hooks t))
      (run-hooks 'doom-exit-buffer-hook)
      (prog1 (apply orig-fn buffer-or-name args)
        (when (buffer-live-p (get-buffer buffer-or-name))
          (with-current-buffer buffer-or-name
            (run-hooks 'doom-enter-buffer-hook)))))))

(defun doom|init-switch-hooks (&optional disable)
  "Set up enter/exit hooks for windows and buffers.

See `doom-enter-buffer-hook', `doom-enter-window-hook', `doom-exit-buffer-hook'
and `doom-exit-window-hook'."
  (dolist (spec '((select-window . doom*switch-window-hooks)
                  (switch-to-buffer . doom*switch-buffer-hooks)
                  (display-buffer . doom*switch-buffer-hooks)
                  (pop-to-buffer . doom*switch-buffer-hooks)))
    (if disable
        (advice-remove (car spec) (cdr spec))
      (advice-add (car spec) :around (cdr spec)))))

(defun doom*load-theme-hooks (theme &rest _)
  "Set up `doom-load-theme-hook' to run after `load-theme' is called."
  (setq doom-theme theme)
  (run-hooks 'doom-load-theme-hook))
(advice-add #'load-theme :after #'doom*load-theme-hooks)


;;
;; Emacs core configuration

;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))     ; pretty
(prefer-coding-system        'utf-8)   ; pretty
(set-terminal-coding-system  'utf-8)   ; pretty
(set-keyboard-coding-system  'utf-8)   ; pretty
(set-selection-coding-system 'utf-8)   ; perdy
(setq locale-coding-system   'utf-8)   ; please
(setq-default buffer-file-coding-system 'utf-8) ; with sugar on top

(setq-default
 ad-redefinition-action 'accept   ; silence advised function warnings
 apropos-do-all t                 ; make `apropos' more useful
 auto-mode-case-fold nil
 autoload-compute-prefixes nil
 debug-on-error doom-debug-mode
 ffap-machine-p-known 'reject     ; don't ping things that look like domain names
 find-file-visit-truename t       ; resolve symlinks when opening files
 idle-update-delay 2              ; update ui less often
 ;; be quiet at startup; don't load or display anything unnecessary
 inhibit-startup-message t
 inhibit-startup-echo-area-message user-login-name
 inhibit-default-init t
 initial-major-mode 'fundamental-mode
 initial-scratch-message nil
 ;; keep the point out of the minibuffer
 minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)
 ;; History & backup settings (save nothing, that's what git is for)
 auto-save-default nil
 create-lockfiles nil
 history-length 500
 make-backup-files nil  ; don't create backup~ files
 ;; byte compilation
 byte-compile-verbose doom-debug-mode
 byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local)
 ;; security
 gnutls-verify-error (not (getenv "INSECURE")) ; you shouldn't use this
 tls-checktrust gnutls-verify-error
 tls-program (list "gnutls-cli --x509cafile %t -p %p %h"
                   ;; compatibility fallbacks
                   "gnutls-cli -p %p %h"
                   "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof")
 ;; Don't store authinfo in plain text!
 auth-sources (list (expand-file-name "authinfo.gpg" doom-etc-dir)
                    "~/.authinfo.gpg")
 ;; files
 abbrev-file-name             (concat doom-local-dir "abbrev.el")
 auto-save-list-file-name     (concat doom-cache-dir "autosave")
 backup-directory-alist       (list (cons "." (concat doom-cache-dir "backup/")))
 pcache-directory             (concat doom-cache-dir "pcache/")
 request-storage-directory    (concat doom-cache-dir "request")
 server-auth-dir              (concat doom-cache-dir "server/")
 shared-game-score-directory  (concat doom-etc-dir "shared-game-score/")
 tramp-auto-save-directory    (concat doom-cache-dir "tramp-auto-save/")
 tramp-backup-directory-alist backup-directory-alist
 tramp-persistency-file-name  (concat doom-cache-dir "tramp-persistency.el")
 url-cache-directory          (concat doom-cache-dir "url/")
 url-configuration-directory  (concat doom-etc-dir "url/")
 gamegrid-user-score-file-directory (concat doom-etc-dir "games/"))

(defvar doom-auto-minor-mode-alist '()
  "Alist mapping filename patterns to corresponding minor mode functions, like
`auto-mode-alist'. All elements of this alist are checked, meaning you can
enable multiple minor modes for the same regexp.")

(defun doom|enable-minor-mode-maybe ()
  "Check file name against `doom-auto-minor-mode-alist'."
  (when buffer-file-name
    (let ((name buffer-file-name)
          (remote-id (file-remote-p buffer-file-name))
          (alist doom-auto-minor-mode-alist))
      ;; Remove backup-suffixes from file name.
      (setq name (file-name-sans-versions name))
      ;; Remove remote file name identification.
      (when (and (stringp remote-id)
                 (string-match (regexp-quote remote-id) name))
        (setq name (substring name (match-end 0))))
      (while (and alist (caar alist) (cdar alist))
        (if (string-match-p (caar alist) name)
            (funcall (cdar alist) 1))
        (setq alist (cdr alist))))))
(add-hook 'find-file-hook #'doom|enable-minor-mode-maybe)

(defun doom*symbol-file (orig-fn symbol &optional type)
  "If a `doom-file' symbol property exists on SYMBOL, use that instead of the
original value of `symbol-file'."
  (or (if (symbolp symbol) (get symbol 'doom-file))
      (funcall orig-fn symbol type)))
(advice-add #'symbol-file :around #'doom*symbol-file)

;; To speed up minibuffer commands (like helm and ivy), defer garbage collection
;; when the minibuffer is active. It may mean a pause when finished, but that's
;; acceptable instead of pauses during.
(defun doom|defer-garbage-collection ()
  (setq gc-cons-threshold doom-gc-cons-upper-limit))
(defun doom|restore-garbage-collection ()
  (setq gc-cons-threshold doom-gc-cons-threshold))
(add-hook 'minibuffer-setup-hook #'doom|defer-garbage-collection)
(add-hook 'minibuffer-exit-hook  #'doom|restore-garbage-collection)

;; File+dir local variables are initialized after the major mode and its hooks
;; have run. If you want hook functions to be aware of these customizations, add
;; them to MODE-local-vars-hook instead.
(defun doom|run-local-var-hooks ()
  "Run MODE-local-vars-hook after local variables are initialized."
  (run-hook-wrapped (intern-soft (format "%s-local-vars-hook" major-mode))
                    #'doom-try-run-hook))
(add-hook 'hack-local-variables-hook #'doom|run-local-var-hooks)


;;
;; Incremental lazy-loading

(defvar doom-incremental-packages '(t)
  "A list of packages to load incrementally after startup. Any large packages
here may cause noticable pauses, so it's recommended you break them up into
sub-packages. For example, `org' is comprised of many packages, and can be broken up into:

  (doom-load-packages-incrementally
   '(calendar find-func format-spec org-macs org-compat
     org-faces org-entities org-list org-pcomplete org-src
     org-footnote org-macro ob org org-clock org-agenda
     org-capture))

This is already done by the lang/org module, however.

If you want to disable incremental loading altogether, either remove
`doom|load-packages-incrementally' from `emacs-startup-hook' or set
`doom-incremental-first-idle-timer' to nil.")

(defvar doom-incremental-first-idle-timer 2
  "How long (in idle seconds) until incremental loading starts.

Set this to nil to disable incremental loading.")

(defvar doom-incremental-idle-timer 1.5
  "How long (in idle seconds) in between incrementally loading packages.")

(defun doom-load-packages-incrementally (packages &optional now)
  "Registers PACKAGES to be loaded incrementally.

If NOW is non-nil, load PACKAGES incrementally, in `doom-incremental-idle-timer'
intervals."
  (if (not now)
      (nconc doom-incremental-packages packages)
    (when packages
      (let ((gc-cons-threshold doom-gc-cons-upper-limit)
            file-name-handler-alist)
        (let* ((reqs (cl-delete-if #'featurep packages))
               (req (ignore-errors (pop reqs))))
          (when req
            (when doom-debug-mode
              (message "Incrementally loading %s" req))
            (condition-case e
                (require req nil t)
              (error
               (message "Failed to load '%s' package incrementally, because: %s"
                        req e)))
            (when reqs
              (run-with-idle-timer doom-incremental-idle-timer
                                   nil #'doom-load-packages-incrementally
                                   reqs t))))))))

(defun doom|load-packages-incrementally ()
  "Begin incrementally loading packages in `doom-incremental-packages'.

If this is a daemon session, load them all immediately instead."
  (if (daemonp)
      (mapc #'require (cdr doom-incremental-packages))
    (when (integerp doom-incremental-first-idle-timer)
      (run-with-idle-timer doom-incremental-first-idle-timer
                           nil #'doom-load-packages-incrementally
                           (cdr doom-incremental-packages) t))))

(add-hook 'emacs-startup-hook #'doom|load-packages-incrementally)


;;
;; Bootstrap helpers

(defun doom-try-run-hook (hook)
  "Run HOOK (a hook function), but handle errors better, to make debugging
issues easier.

Meant to be used with `run-hook-wrapped'."
  (when doom-debug-mode
    (message "Running doom hook: %s" hook))
  (condition-case e
      (funcall hook)
    ((debug error)
     (signal 'doom-hook-error (list hook e))))
  ;; return nil so `run-hook-wrapped' won't short circuit
  nil)

(defun doom-ensure-same-emacs-version-p ()
  "Check if the running version of Emacs has changed and set
`doom-emacs-changed-p' if it has."
  (if (load doom--last-emacs-file 'noerror 'nomessage 'nosuffix)
      (setq doom-emacs-changed-p
            (not (equal emacs-version doom--last-emacs-version)))
    (with-temp-file doom--last-emacs-file
      (princ `(setq doom--last-emacs-version ,(prin1-to-string emacs-version))
             (current-buffer))))
  (cond ((not doom-emacs-changed-p))
        ((y-or-n-p
          (format
           (concat "Your version of Emacs has changed from %s to %s, which may cause incompatibility\n"
                   "issues. If you run into errors, run `bin/doom compile :plugins` or reinstall your\n"
                   "plugins to resolve them.\n\n"
                   "Continue?")
           doom--last-emacs-version
           emacs-version))
         (delete-file doom--last-emacs-file))
        (noninteractive (error "Aborting"))
        ((kill-emacs))))

(defun doom-ensure-core-directories-exist ()
  "Make sure all Doom's essential local directories (in and including
`doom-local-dir') exist."
  (dolist (dir (list doom-local-dir doom-etc-dir doom-cache-dir doom-packages-dir))
    (unless (file-directory-p dir)
      (make-directory dir t))))

(defun doom|display-benchmark (&optional return-p)
  "Display a benchmark, showing number of packages and modules, and how quickly
they were loaded at startup.

If RETURN-P, return the message as a string instead of displaying it."
  (funcall (if return-p #'format #'message)
           "Doom loaded %s packages across %d modules in %.03fs"
           (length package-activated-list)
           (if doom-modules (hash-table-count doom-modules) 0)
           (or doom-init-time
               (setq doom-init-time (float-time (time-subtract (current-time) before-init-time))))))

(defun doom|run-all-startup-hooks ()
  "Run all startup Emacs hooks. Meant to be executed after starting Emacs with
-q or -Q, for example:

  emacs -Q -l init.el -f doom|run-all-startup-hooks"
  (run-hook-wrapped 'after-init-hook #'doom-try-run-hook)
  (setq after-init-time (current-time))
  (dolist (hook (list 'delayed-warnings-hook
                      'emacs-startup-hook 'term-setup-hook
                      'window-setup-hook))
    (run-hook-wrapped hook #'doom-try-run-hook)))


;;
;; Bootstrap functions

(defun doom-initialize (&optional force-p force-load-core-p)
  "Bootstrap Doom, if it hasn't already (or if FORCE-P is non-nil).

Loads Doom core files if in an interactive session or FORCE-LOAD-CORE-P is
non-nil.

The bootstrap process involves making sure 1) the essential directories exist,
2) the core packages are installed, 3) `doom-autoload-file' and
`doom-package-autoload-file' exist and have been loaded, and 4) Doom's core
files are loaded.

If the cache exists, much of this function isn't run, which substantially
reduces startup time.

The overall load order of Doom is as follows:

  ~/.emacs.d/init.el
  ~/.emacs.d/core/core.el
  ~/.doom.d/init.el
  Module init.el files
  `doom-init-hook'
  Module config.el files
  ~/.doom.d/config.el
  `doom-post-init-hook'
  `after-init-hook'
  `emacs-startup-hook'

Module load order is determined by your `doom!' block. See `doom-modules-dirs'
for a list of all recognized module trees. Order defines precedence (from most
to least)."
  (when (or force-p (not doom-init-p))
    (setq doom-init-p t)  ; Prevent infinite recursion

    ;; `doom-autoload-file' tells Emacs where to load all its autoloaded
    ;; functions from. This includes everything in core/autoload/*.el and all
    ;; the autoload files in your enabled modules.
    (when (or force-p (not (doom-initialize-autoloads doom-autoload-file)))
      (doom-ensure-core-directories-exist)
      (doom-ensure-same-emacs-version-p)

      (require 'core-packages)
      (doom-ensure-packages-initialized force-p)
      (doom-ensure-core-packages)

      (unless (or force-p noninteractive)
        (user-error "Your doom autoloads are missing! Run `bin/doom refresh' to regenerate them")))

    ;; Loads `doom-package-autoload-file', which loads a concatenated package
    ;; autoloads file and caches `load-path', `auto-mode-alist',
    ;; `Info-directory-list', `doom-disabled-packages' and
    ;; `package-activated-list'. A big reduction in startup time.
    (unless (or force-p
                (doom-initialize-autoloads doom-package-autoload-file)
                noninteractive)
      (user-error "Your package autoloads are missing! Run `bin/doom refresh' to regenerate them")))

  (require 'core-os)
  (when (or force-load-core-p (not noninteractive))
    (add-hook! 'emacs-startup-hook
      #'(doom|init-switch-hooks doom|display-benchmark))

    (require 'core-ui)
    (require 'core-editor)
    (require 'core-projects)
    (require 'core-keybinds)))

(defun doom-initialize-autoloads (file)
  "Tries to load FILE (an autoloads file). Return t on success, throws an error
in interactive sessions, nil otherwise (but logs a warning)."
  (condition-case e
      (load (file-name-sans-extension file) 'noerror 'nomessage)
    ((debug error)
     (if noninteractive
         (message "Autoload file warning: %s -> %s" (car e) (error-message-string e))
       (signal 'doom-autoload-error (list (file-name-nondirectory file) e))))))


;;
;; Bootstrap Doom

(add-to-list 'load-path doom-core-dir)

(require 'core-lib)
(require 'core-modules)
(when noninteractive
  (require 'core-cli))
(after! package
  (require 'core-packages))

(doom-initialize noninteractive)
(unless noninteractive
  (doom-initialize-modules))

(provide 'core)
;;; core.el ends here
