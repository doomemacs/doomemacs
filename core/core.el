;;; core.el --- the heart of the beast -*- lexical-binding: t; -*-

(eval-when-compile
  (when (< emacs-major-version 26)
    (error "Detected Emacs v%s. Doom only supports Emacs 26 and newer"
           emacs-version)))


;;
;;; Variables

(defconst doom-version "2.0.9"
  "Current version of Doom Emacs.")

(defconst EMACS27+   (> emacs-major-version 26))
(defconst EMACS28+   (> emacs-major-version 27))
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))

;; Unix tools look for HOME, but this is normally not defined on Windows.
(when (and IS-WINDOWS (null (getenv "HOME")))
  (setenv "HOME" (getenv "USERPROFILE")))

;; Ensure `doom-core-dir' is in `load-path'
(add-to-list 'load-path (file-name-directory load-file-name))

(defvar doom--initial-load-path load-path)
(defvar doom--initial-process-environment process-environment)
(defvar doom--initial-exec-path exec-path)

;; `file-name-handler-alist' is consulted on every `require', `load' and various
;; path/io functions. You get a minor speed up by nooping this. However, this
;; may cause problems on builds of Emacs where its site lisp files aren't
;; byte-compiled and we're forced to load the *.el.gz files (e.g. on Alpine)
(unless noninteractive
  (defvar doom--initial-file-name-handler-alist file-name-handler-alist)

  (setq file-name-handler-alist nil)
  ;; Restore `file-name-handler-alist', because it is needed for handling
  ;; encrypted or compressed files, among other things.
  (defun doom-reset-file-handler-alist-h ()
    ;; Re-add rather than `setq', because file-name-handler-alist may have
    ;; changed since startup, and we want to preserve those.
    (dolist (handler file-name-handler-alist)
      (add-to-list 'doom--initial-file-name-handler-alist handler))
    (setq file-name-handler-alist doom--initial-file-name-handler-alist))
  (add-hook 'emacs-startup-hook #'doom-reset-file-handler-alist-h))

;; REVIEW Fixes 'void-variable tab-prefix-map' errors caused by packages that
;;        prematurely use this variable before it was introduced. Remove this in
;;        a year.
(unless (boundp 'tab-prefix-map)
  (defvar tab-prefix-map (make-sparse-keymap)))

;; Just the bare necessities
(require 'subr-x)
(require 'cl-lib)
(require 'core-lib)


;;
;;; Global variables

(defvar doom-init-p nil
  "Non-nil if Doom has been initialized.")

(defvar doom-init-time nil
  "The time it took, in seconds, for Doom Emacs to initialize.")

(defvar doom-debug-p (or (getenv "DEBUG") init-file-debug)
  "If non-nil, Doom will log more.

Use `doom-debug-mode' to toggle it. The --debug-init flag and setting the DEBUG
envvar will enable this at startup.")

(defvar doom-interactive-p (not noninteractive)
  "If non-nil, Emacs is in interactive mode.")

;;; Directories/files
(defconst doom-emacs-dir
  (eval-when-compile (file-truename user-emacs-directory))
  "The path to the currently loaded .emacs.d directory. Must end with a slash.")

(defconst doom-core-dir (concat doom-emacs-dir "core/")
  "The root directory of Doom's core files. Must end with a slash.")

(defconst doom-modules-dir (concat doom-emacs-dir "modules/")
  "The root directory for Doom's modules. Must end with a slash.")

(defconst doom-local-dir
  (if-let (localdir (getenv "DOOMLOCALDIR"))
      (expand-file-name (file-name-as-directory localdir))
    (concat doom-emacs-dir ".local/"))
  "Root directory for local storage.

Use this as a storage location for this system's installation of Doom Emacs.
These files should not be shared across systems. By default, it is used by
`doom-etc-dir' and `doom-cache-dir'. Must end with a slash.")

(defconst doom-etc-dir (concat doom-local-dir "etc/")
  "Directory for non-volatile local storage.

Use this for files that don't change much, like server binaries, external
dependencies or long-term shared data. Must end with a slash.")

(defconst doom-cache-dir (concat doom-local-dir "cache/")
  "Directory for volatile local storage.

Use this for files that change often, like cache files. Must end with a slash.")

(defconst doom-docs-dir (concat doom-emacs-dir "docs/")
  "Where Doom's documentation files are stored. Must end with a slash.")

(defconst doom-private-dir
  (if-let (doomdir (getenv "DOOMDIR"))
      (expand-file-name (file-name-as-directory doomdir))
    (or (let ((xdgdir
               (expand-file-name "doom/"
                                 (or (getenv "XDG_CONFIG_HOME")
                                     "~/.config"))))
          (if (file-directory-p xdgdir) xdgdir))
        "~/.doom.d/"))
  "Where your private configuration is placed.

Defaults to ~/.config/doom, ~/.doom.d or the value of the DOOMDIR envvar;
whichever is found first. Must end in a slash.")

(defconst doom-autoload-file (concat doom-local-dir "autoloads.el")
  "Where `doom-reload-core-autoloads' stores its core autoloads.

This file is responsible for informing Emacs where to find all of Doom's
autoloaded core functions (in core/autoload/*.el).")

(defconst doom-env-file (concat doom-local-dir "env")
  "The location of your envvar file, generated by `doom env`.

This file contains environment variables scraped from your shell environment,
which is loaded at startup (if it exists). This is helpful if Emacs can't
\(easily) be launched from the correct shell session (particularly for MacOS
users).")

;;; Custom error types
(define-error 'doom-error "Error in Doom Emacs core")
(define-error 'doom-hook-error "Error in a Doom startup hook" 'doom-error)
(define-error 'doom-autoload-error "Error in Doom's autoloads file" 'doom-error)
(define-error 'doom-module-error "Error in a Doom module" 'doom-error)
(define-error 'doom-private-error "Error in private config" 'doom-error)
(define-error 'doom-package-error "Error with packages" 'doom-error)


;;
;;; Emacs core configuration

;; lo', longer logs ahoy, so to reliably locate lapses in doom's logic later
(setq message-log-max 8192)

;; Reduce debug output, well, unless we've asked for it.
(setq debug-on-error doom-debug-p
      jka-compr-verbose doom-debug-p)

;; Contrary to what many Emacs users have in their configs, you really don't
;; need more than this to make UTF-8 the default coding system:
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))       ; pretty
(prefer-coding-system 'utf-8)            ; pretty
(setq locale-coding-system 'utf-8)       ; please
;; The clipboard's on Windows could be in a wider (or thinner) encoding than
;; utf-8 (likely UTF-16), so let Emacs/the OS decide what encoding to use there.
(unless IS-WINDOWS
  (setq selection-coding-system 'utf-8)) ; with sugar on top

;; Disable warnings from legacy advice system. They aren't useful, and what can
;; we do about them, besides changing packages upstream?
(setq ad-redefinition-action 'accept)

;; Make `apropos' et co search more extensively. They're more useful this way.
(setq apropos-do-all t)

;; A second, case-insensitive pass over `auto-mode-alist' is time wasted, and
;; indicates misconfiguration (or that the user needs to stop relying on case
;; insensitivity).
(setq auto-mode-case-fold nil)

;; Reduce *Message* noise at startup. An empty scratch buffer (or the dashboard)
;; is more than enough.
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      ;; Shave seconds off startup time by starting the scratch buffer in
      ;; `fundamental-mode', rather than, say, `org-mode' or `text-mode', which
      ;; pull in a ton of packages.
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;; Get rid of "For information about GNU Emacs..." message at startup, unless
;; we're in a daemon session where it'll say "Starting Emacs daemon." instead,
;; which isn't so bad.
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

;; Emacs "updates" its ui more often than it needs to, so we slow it down
;; slightly from 0.5s:
(setq idle-update-delay 1.0)

;; Emacs is essentially one huge security vulnerability, what with all the
;; dependencies it pulls in from all corners of the globe. Let's try to be at
;; least a little more discerning.
(setq gnutls-verify-error (not (getenv "INSECURE"))
      gnutls-algorithm-priority
      (when (boundp 'libgnutls-version)
        (concat "SECURE128:+SECURE192:-VERS-ALL"
                (if (and (not IS-WINDOWS)
                         (not (version< emacs-version "26.3"))
                         (>= libgnutls-version 30605))
                    ":+VERS-TLS1.3")
                ":+VERS-TLS1.2"))
      ;; `gnutls-min-prime-bits' is set based on recommendations from
      ;; https://www.keylength.com/en/4/
      gnutls-min-prime-bits 3072
      tls-checktrust gnutls-verify-error
      ;; Emacs is built with `gnutls' by default, so `tls-program' would not be
      ;; used in that case. Otherwise, people have reasons to not go with
      ;; `gnutls', we use `openssl' instead. For more details, see
      ;; https://redd.it/8sykl1
      tls-program '("openssl s_client -connect %h:%p -CAfile %t -nbio -no_ssl3 -no_tls1 -no_tls1_1 -ign_eof"
                    "gnutls-cli -p %p --dh-bits=3072 --ocsp --x509cafile=%t \
--strict-tofu --priority='SECURE192:+SECURE128:-VERS-ALL:+VERS-TLS1.2:+VERS-TLS1.3' %h"
                    ;; compatibility fallbacks
                    "gnutls-cli -p %p %h"))

;; Emacs stores `authinfo' in $HOME and in plain-text. Let's not do that, mkay?
;; This file stores usernames, passwords, and other such treasures for the
;; aspiring malicious third party.
(setq auth-sources (list (concat doom-etc-dir "authinfo.gpg")
                         "~/.authinfo.gpg"))

;; Don't litter `doom-emacs-dir'. We don't use `no-littering' because it's a
;; mote too opinionated for our needs.
(setq abbrev-file-name             (concat doom-local-dir "abbrev.el")
      async-byte-compile-log-file  (concat doom-etc-dir "async-bytecomp.log")
      bookmark-default-file        (concat doom-etc-dir "bookmarks")
      custom-file                  (concat doom-private-dir "custom.el")
      custom-theme-directory       (concat doom-private-dir "themes/")
      desktop-dirname              (concat doom-etc-dir "desktop")
      desktop-base-file-name       "autosave"
      desktop-base-lock-name       "autosave-lock"
      pcache-directory             (concat doom-cache-dir "pcache/")
      request-storage-directory    (concat doom-cache-dir "request")
      shared-game-score-directory  (concat doom-etc-dir "shared-game-score/")
      tramp-auto-save-directory    (concat doom-cache-dir "tramp-auto-save/")
      tramp-backup-directory-alist backup-directory-alist
      tramp-persistency-file-name  (concat doom-cache-dir "tramp-persistency.el")
      url-cache-directory          (concat doom-cache-dir "url/")
      url-configuration-directory  (concat doom-etc-dir "url/")
      gamegrid-user-score-file-directory (concat doom-etc-dir "games/"))

;; HACK Stop sessions from littering the user directory
(defadvice! doom--use-cache-dir-a (session-id)
  :override #'emacs-session-filename
  (concat doom-cache-dir "emacs-session." session-id))

(defadvice! doom--save-enabled-commands-to-doomdir-a (orig-fn &rest args)
  "When enabling a disabled command, the `put' call is written to
~/.emacs.d/init.el, which causes issues for Doom, so write it to the user's
config.el instead."
  :around #'en/disable-command
  (let ((user-init-file custom-file))
    (apply orig-fn args)))


;;
;;; Optimizations

;; Disable bidirectional text rendering for a modest performance boost. I've set
;; this to `nil' in the past, but the `bidi-display-reordering's docs say that
;; is an undefined state and suggest this to be just as good:
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Disabling the BPA makes redisplay faster, but might produce incorrect display
;; reordering of bidirectional text with embedded parentheses and other bracket
;; characters whose 'paired-bracket' Unicode property is non-nil.
(setq bidi-inhibit-bpa t)  ; Emacs 27 only

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).
(setq frame-inhibit-implied-resize t)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether it has a notable affect on Linux and Mac hasn't
;; been determined, but we inhibit it there anyway.
(setq inhibit-compacting-font-caches t)

;; Performance on Windows is considerably worse than elsewhere. We'll need
;; everything we can get.
(when IS-WINDOWS
  (setq w32-get-true-file-attributes nil   ; decrease file IO workload
        w32-pipe-read-delay 0              ; faster ipc
        w32-pipe-buffer-size (* 64 1024))) ; read more at a time (was 4K)

;; Remove command line options that aren't relevant to our current OS; means
;; slightly less to process at startup.
(unless IS-MAC   (setq command-line-ns-option-alist nil))
(unless IS-LINUX (setq command-line-x-option-alist nil))

;; Delete files to trash on macOS, as an extra layer of precaution against
;; accidentally deleting wanted files.
(setq delete-by-moving-to-trash IS-MAC)

;; Adopt a sneaky garbage collection strategy of waiting until idle time to
;; collect; staving off the collector while the user is working.
(setq gcmh-idle-delay 5
      gcmh-high-cons-threshold (* 16 1024 1024)  ; 16mb
      gcmh-verbose doom-debug-p)

;; HACK `tty-run-terminal-initialization' is *tremendously* slow for some
;;      reason. Disabling it completely could have many side-effects, so we
;;      defer it until later, at which time it (somehow) runs very quickly.
(unless (daemonp)
  (advice-add #'tty-run-terminal-initialization :override #'ignore)
  (add-hook! 'window-setup-hook
    (defun doom-init-tty-h ()
      (advice-remove #'tty-run-terminal-initialization #'ignore)
      (tty-run-terminal-initialization (selected-frame) nil t))))


;;
;;; MODE-local-vars-hook

;; File+dir local variables are initialized after the major mode and its hooks
;; have run. If you want hook functions to be aware of these customizations, add
;; them to MODE-local-vars-hook instead.
(defvar doom--inhibit-local-var-hooks nil)

(defun doom-run-local-var-hooks-h ()
  "Run MODE-local-vars-hook after local variables are initialized."
  (unless doom--inhibit-local-var-hooks
    (set (make-local-variable 'doom--inhibit-local-var-hooks) t)
    (run-hook-wrapped (intern-soft (format "%s-local-vars-hook" major-mode))
                      #'doom-try-run-hook)))

;; If the user has disabled `enable-local-variables', then
;; `hack-local-variables-hook' is never triggered, so we trigger it at the end
;; of `after-change-major-mode-hook':
(defun doom-run-local-var-hooks-maybe-h ()
  "Run `doom-run-local-var-hooks-h' if `enable-local-variables' is disabled."
  (unless enable-local-variables
    (doom-run-local-var-hooks-h)))


;;
;;; Incremental lazy-loading

(defvar doom-incremental-packages '(t)
  "A list of packages to load incrementally after startup. Any large packages
here may cause noticable pauses, so it's recommended you break them up into
sub-packages. For example, `org' is comprised of many packages, and can be
broken up into:

  (doom-load-packages-incrementally
   '(calendar find-func format-spec org-macs org-compat
     org-faces org-entities org-list org-pcomplete org-src
     org-footnote org-macro ob org org-clock org-agenda
     org-capture))

This is already done by the lang/org module, however.

If you want to disable incremental loading altogether, either remove
`doom-load-packages-incrementally-h' from `emacs-startup-hook' or set
`doom-incremental-first-idle-timer' to nil. Incremental loading does not occur
in daemon sessions (they are loaded immediately at startup).")

(defvar doom-incremental-first-idle-timer 2.0
  "How long (in idle seconds) until incremental loading starts.

Set this to nil to disable incremental loading.")

(defvar doom-incremental-idle-timer 0.75
  "How long (in idle seconds) in between incrementally loading packages.")

(defvar doom-incremental-load-immediately (daemonp)
  "If non-nil, load all incrementally deferred packages immediately at startup.")

(defun doom-load-packages-incrementally (packages &optional now)
  "Registers PACKAGES to be loaded incrementally.

If NOW is non-nil, load PACKAGES incrementally, in `doom-incremental-idle-timer'
intervals."
  (if (not now)
      (appendq! doom-incremental-packages packages)
    (while packages
      (let ((req (pop packages)))
        (unless (featurep req)
          (doom-log "Incrementally loading %s" req)
          (condition-case e
              (or (while-no-input
                    ;; If `default-directory' is a directory that doesn't exist
                    ;; or is unreadable, Emacs throws up file-missing errors, so
                    ;; we set it to a directory we know exists and is readable.
                    (let ((default-directory doom-emacs-dir)
                          (gc-cons-threshold most-positive-fixnum)
                          file-name-handler-alist)
                      (require req nil t))
                    t)
                  (push req packages))
            ((error debug)
             (message "Failed to load %S package incrementally, because: %s"
                      req e)))
          (if (not packages)
              (doom-log "Finished incremental loading")
            (run-with-idle-timer doom-incremental-idle-timer
                                 nil #'doom-load-packages-incrementally
                                 packages t)
            (setq packages nil)))))))

(defun doom-load-packages-incrementally-h ()
  "Begin incrementally loading packages in `doom-incremental-packages'.

If this is a daemon session, load them all immediately instead."
  (if doom-incremental-load-immediately
      (mapc #'require (cdr doom-incremental-packages))
    (when (numberp doom-incremental-first-idle-timer)
      (run-with-idle-timer doom-incremental-first-idle-timer
                           nil #'doom-load-packages-incrementally
                           (cdr doom-incremental-packages) t))))


;;
;;; Custom hooks

(defvar doom-first-input-hook nil
  "Transient hooks run before the first user input.")

(defvar doom-first-file-hook nil
  "Transient hooks run before the first interactively opened file.")

(defvar doom-first-buffer-hook nil
  "Transient hooks run before the first interactively opened buffer.")


;;
;;; Bootstrap helpers

(defun doom-display-benchmark-h (&optional return-p)
  "Display a benchmark including number of packages and modules loaded.

If RETURN-P, return the message as a string instead of displaying it."
  (funcall (if return-p #'format #'message)
           "Doom loaded %d packages across %d modules in %.03fs"
           (- (length load-path) (length doom--initial-load-path))
           (if doom-modules (hash-table-count doom-modules) 0)
           (or doom-init-time
               (setq doom-init-time
                     (float-time (time-subtract (current-time) before-init-time))))))

(defun doom-initialize (&optional force-p)
  "Bootstrap Doom, if it hasn't already (or if FORCE-P is non-nil).

The bootstrap process ensures that everything Doom needs to run is set up;
essential directories exist, core packages are installed, `doom-autoload-file'
is loaded (failing if it isn't), that all the needed hooks are in place, and
that `core-packages' will load when `package' or `straight' is used.

The overall load order of Doom is as follows:

  ~/.emacs.d/init.el
  ~/.emacs.d/core/core.el
  ~/.doom.d/init.el
  Module init.el files
  `doom-before-init-modules-hook'
  Module config.el files
  ~/.doom.d/config.el
  `doom-init-modules-hook'
  `doom-after-init-modules-hook' (`after-init-hook')
  `emacs-startup-hook'
  `doom-init-ui-hook'
  `window-setup-hook'

Module load order is determined by your `doom!' block. See `doom-modules-dirs'
for a list of all recognized module trees. Order defines precedence (from most
to least)."
  (when (or force-p (not doom-init-p))
    (setq doom-init-p t)
    (doom-log "Initializing Doom")

    ;; Reset as much state as possible, so `doom-initialize' can be treated like
    ;; a reset function. e.g. when reloading the config.
    (setq-default exec-path doom--initial-exec-path
                  load-path doom--initial-load-path
                  process-environment doom--initial-process-environment)

    ;; Doom caches a lot of information in `doom-autoload-file'. Module and
    ;; package autoloads, autodefs like `set-company-backend!', and variables
    ;; like `doom-modules', `doom-disabled-packages', `load-path',
    ;; `auto-mode-alist', and `Info-directory-list'. etc. Compiling them into
    ;; one place is a big reduction in startup time.
    (condition-case e
        ;; Avoid `file-name-sans-extension' for premature optimization reasons.
        ;; `string-remove-suffix' is cheaper because it performs no file sanity
        ;; checks; just plain ol' string manipulation.
        (load (string-remove-suffix ".el" doom-autoload-file)
              nil 'nomessage)
      (file-missing
       ;; If the autoloads file fails to load then the user forgot to sync, or
       ;; aborted a doom command midway!
       (if (equal (nth 3 e) doom-autoload-file)
           (signal 'doom-error
                   (list "Doom is in an incomplete state"
                         "run 'bin/doom sync' on the command line to repair it"))
         ;; Otherwise, something inside the autoloads file is triggering this
         ;; error; forward it!
         (apply #'doom-autoload-error e))))

    ;; Load shell environment, optionally generated from 'doom env'. No need
    ;; to do so if we're in terminal Emacs, where Emacs correctly inherits
    ;; your shell environment.
    (if (or (display-graphic-p)
            (daemonp))
        (doom-load-envvars-file doom-env-file 'noerror))

    ;; Loads `use-package' and all the helper macros modules (and users) can use
    ;; to configure their packages.
    (require 'core-modules)

    ;; There's a chance the user will later use package.el or straight in this
    ;; interactive session. If they do, make sure they're properly initialized
    ;; when they do.
    (autoload 'doom-initialize-packages "core-packages")
    (autoload 'doom-initialize-core-packages "core-packages")
    (with-eval-after-load 'package (require 'core-packages))
    (with-eval-after-load 'straight (doom-initialize-packages))

    ;; Bootstrap the interactive session
    (add-hook! 'window-setup-hook
      (add-hook 'hack-local-variables-hook #'doom-run-local-var-hooks-h)
      (add-hook 'after-change-major-mode-hook #'doom-run-local-var-hooks-maybe-h 'append)
      (add-hook 'doom-first-input-hook #'gcmh-mode)
      (add-hook-trigger! 'doom-first-input-hook 'pre-command-hook)
      (add-hook-trigger! 'doom-first-file-hook 'after-find-file 'dired-initial-position-hook)
      (add-hook-trigger! 'doom-first-buffer-hook 'after-find-file 'doom-switch-buffer-hook))
    (add-hook 'emacs-startup-hook #'doom-load-packages-incrementally-h)
    (add-hook 'window-setup-hook #'doom-display-benchmark-h 'append)
    (if doom-debug-p (doom-debug-mode +1))

    ;; Load core/core-*.el, the user's private init.el, then their config.el
    (doom-initialize-modules force-p))

  doom-init-p)

(provide 'core)
;;; core.el ends here
