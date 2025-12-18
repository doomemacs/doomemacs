;;; doom.el --- the heart of the beast -*- lexical-binding: t; -*-
;;
;; Author:  Henrik Lissner <contact@henrik.io>
;; URL:     https://github.com/doomemacs/doomemacs
;;
;;   =================     ===============     ===============   ========  ========
;;   \\ . . . . . . .\\   //. . . . . . .\\   //. . . . . . .\\  \\. . .\\// . . //
;;   ||. . ._____. . .|| ||. . ._____. . .|| ||. . ._____. . .|| || . . .\/ . . .||
;;   || . .||   ||. . || || . .||   ||. . || || . .||   ||. . || ||. . . . . . . ||
;;   ||. . ||   || . .|| ||. . ||   || . .|| ||. . ||   || . .|| || . | . . . . .||
;;   || . .||   ||. _-|| ||-_ .||   ||. . || || . .||   ||. _-|| ||-_.|\ . . . . ||
;;   ||. . ||   ||-'  || ||  `-||   || . .|| ||. . ||   ||-'  || ||  `|\_ . .|. .||
;;   || . _||   ||    || ||    ||   ||_ . || || . _||   ||    || ||   |\ `-_/| . ||
;;   ||_-' ||  .|/    || ||    \|.  || `-_|| ||_-' ||  .|/    || ||   | \  / |-_.||
;;   ||    ||_-'      || ||      `-_||    || ||    ||_-'      || ||   | \  / |  `||
;;   ||    `'         || ||         `'    || ||    `'         || ||   | \  / |   ||
;;   ||            .===' `===.         .==='.`===.         .===' /==. |  \/  |   ||
;;   ||         .=='   \_|-_ `===. .==='   _|_   `===. .===' _-|/   `==  \/  |   ||
;;   ||      .=='    _-'    `-_  `='    _-'   `-_    `='  _-'   `-_  /|  \/  |   ||
;;   ||   .=='    _-'          '-__\._-'         '-_./__-'         `' |. /|  |   ||
;;   ||.=='    _-'                                                     `' |  /==.||
;;   =='    _-'                                                            \/   `==
;;   \   _-'                                                                `-_   /
;;    `''                                                                      ``'
;;
;; These demons are not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This is Doom's heart, where I define all its major constants and variables,
;; set its saner global defaults, then prepare Emacs to bootstrap Doom.
;;
;; The overall load order of Doom is as follows:
;;
;;   > $EMACSDIR/early-init.el
;;     > $EMACSDIR/lisp/doom.el
;;       - $EMACSDIR/lisp/doom-lib.el
;;     > $EMACSDIR/lisp/doom-start.el
;;       - hook: `doom-before-init-hook'
;;       - $DOOMDIR/init.el
;;   - hook: `before-init-hook'
;;   > $XDG_DATA_HOME/doom/$PROFILE/@/$VERSION/init.el   (replaces $EMACSDIR/init.el)
;;     - $EMACSDIR/doom-{keybinds,ui,projects,editor}.el
;;     - hook: `doom-before-modules-init-hook'
;;     - {$DOOMDIR,$EMACSDIR}/modules/*/*/init.el
;;     - hook: `doom-after-modules-init-hook'
;;     - hook: `doom-before-modules-config-hook'
;;     - {$DOOMDIR,$EMACSDIR}/modules/*/*/config.el
;;     - hook: `doom-after-modules-config-hook'
;;     - $DOOMDIR/config.el
;;     - `custom-file' or $DOOMDIR/custom.el
;;   - hook: `after-init-hook'
;;   - hook: `emacs-startup-hook'
;;   - hook: `window-setup-hook'
;;   - hook: `doom-init-ui-hook'
;;   - hook: `doom-after-init-hook'
;;   > After startup is complete (if file(s) have been opened from the command
;;     line, these will trigger much earlier):
;;     - On first input:              `doom-first-input-hook'
;;     - On first switched-to buffer: `doom-first-buffer-hook'
;;     - On first opened file:        `doom-first-file-hook'
;;
;; This file is Doom's heart, where I define all its major constants and
;; variables, set only its sanest global defaults, employ its hackiest (and
;; least offensive) optimizations, and load the minimum needed for all Doom
;; sessions, interactive or otherwise.
;;
;; See doom-start.el for initialization intended solely for interactive
;; sessions, and doom-cli.el for non-interactive sessions.
;;
;;; Code:

;; For the `when-let*' and `if-let*' macros on older versions of Emacs, before
;; they were autoloaded.
(eval-when-compile (require 'subr-x))

(eval-and-compile
  ;; Doom core supports Emacs 27.1+, but its official module library requires
  ;; 28.1+. Also keep in mind that certain modules may have stricter
  ;; requirements (e.g.  tree-sitter needs 29.1+).
  (when (< emacs-major-version 27)
    (user-error
     (concat
      "Detected Emacs " emacs-version ", but Doom requires 27.1 or newer (30.2 is\n\n"
      "recommended). The current Emacs executable in use is:\n\n  " (car command-line-args)
      "\n\nA guide for installing a newer version of Emacs can be found at:\n\n  "
      (format "https://docs.doomemacs.org/-/install/%s"
              (cond ((eq system-type 'darwin) "on-macos")
                    ((memq system-type '(cygwin windows-nt ms-dos)) "on-windows")
                    ("on-linux")))
      "\n\n"
      (if noninteractive
          (concat "Alternatively, either update your $PATH environment variable to include the\n"
                  "path of the desired Emacs executable OR alter the $EMACS environment variable\n"
                  "to specify the exact path or command needed to invoke Emacs."
                  (when-let ((script (cadr (member "--load" command-line-args)))
                             (command (file-name-nondirectory script)))
                    (concat " For example:\n\n"
                            "  $ EMACS=/path/to/valid/emacs " command " ...\n"
                            "  $ EMACS=\"/Applications/Emacs.app/Contents/MacOS/Emacs\" " command " ...\n"
                            "  $ EMACS=\"snap run emacs\" " command " ..."))
                  "\n\nAborting...")
        (concat "If you believe this error is a mistake, run 'doom doctor' on the command line\n"
                "to diagnose common issues with your config and system.")))))
  nil)

;; Doom needs to be synced/rebuilt if either Doom or Emacs has been
;; up/downgraded. This is because byte-code isn't backwards compatible, and many
;; packages (including Doom), bake in absolute paths into their caches that need
;; to be refreshed.
(let ((old-version (eval-when-compile emacs-major-version)))
  (unless (= emacs-major-version old-version)
    (user-error (concat "Doom was compiled with Emacs %s, but was loaded with %s. Run 'doom sync' to"
                        "recompile it.")
                emacs-major-version old-version)))

;;; Custom features & global constants
;; Doom has its own features that its modules, CLI, and user extensions can
;; announce, and don't belong in `features', so they are stored here, which can
;; include information about the external system environment.
(defconst doom-system
  (pcase system-type
    ('darwin                           '(macos bsd))
    ((or 'cygwin 'windows-nt 'ms-dos)  '(windows))
    ((or 'gnu 'gnu/linux)              '(linux))
    ((or 'gnu/kfreebsd 'berkeley-unix) '(linux bsd))
    ('android                          '(android)))
  "A list of symbols denoting available features in the active Doom profile.")

;; Convenience aliases for internal use only (may be removed later).
(defconst doom--system-windows-p (eq 'windows (car doom-system)))
(defconst doom--system-macos-p   (eq 'macos   (car doom-system)))
(defconst doom--system-linux-p   (eq 'linux   (car doom-system)))

;; Announce WSL if it is detected.
(when (and doom--system-linux-p
           (if (boundp 'operating-system-release) ; is deprecated since 28.x
               (string-match-p "-[Mm]icrosoft" operating-system-release)
             (getenv-internal "WSLENV")))
  (add-to-list 'doom-system 'wsl 'append))

;; `system-type' is esoteric, so I create a pseudo feature as a stable and
;; consistent alternative, for use with `featurep'.
(push :system features)
(put :system 'subfeatures doom-system)

;; Emacs needs a more consistent way to detect build features, and the docs
;; claim `system-configuration-features' is not da way. Some features (that
;; don't represent packages) can be found in `features' (which `featurep'
;; consults), but aren't consistent, so I'll impose some consistency:
(if (bound-and-true-p module-file-suffix)
    (push 'dynamic-modules features))
(if (fboundp #'json-parse-string)
    (push 'jansson features))
(if (string-match-p "HARFBUZZ" system-configuration-features) ; no alternative
    (push 'harfbuzz features))

;; The `native-compile' feature exists whether or not it is functional (e.g.
;; libgcc is available or not). This seems silly, as some packages will blindly
;; use the native-comp API if it's present but non-functional, so let's pretend
;; it doesn't exist if that's the case.
(if (featurep 'native-compile)
    (if (not (native-comp-available-p))
        (delq 'native-compile features)))

;; DEPRECATED: Remove in v3
(with-no-warnings
  (defconst IS-MAC      doom--system-macos-p)
  (defconst IS-LINUX    doom--system-linux-p)
  (defconst IS-WINDOWS  doom--system-windows-p)
  (defconst IS-BSD      (memq 'bsd doom-system))
  (defconst EMACS28+    (> emacs-major-version 27))
  (defconst EMACS29+    (> emacs-major-version 28))
  (defconst MODULES     (featurep 'dynamic-modules))
  (defconst NATIVECOMP  (featurep 'native-compile))

  (make-obsolete-variable 'IS-MAC     "Use (featurep :system 'macos) instead" "3.0.0")
  (make-obsolete-variable 'IS-LINUX   "Use (featurep :system 'linux) instead" "3.0.0")
  (make-obsolete-variable 'IS-WINDOWS "Use (featurep :system 'windows) instead" "3.0.0")
  (make-obsolete-variable 'IS-BSD     "Use (featurep :system 'bsd) instead" "3.0.0")
  (make-obsolete-variable 'EMACS28+   "Use (>= emacs-major-version 28) instead" "3.0.0")
  (make-obsolete-variable 'EMACS29+   "Use (>= emacs-major-version 29) instead" "3.0.0")
  (make-obsolete-variable 'MODULES    "Use (featurep 'dynamic-modules) instead" "3.0.0")
  (make-obsolete-variable 'NATIVECOMP "Use (featurep 'native-compile) instead" "3.0.0")

  (define-obsolete-variable-alias 'doom-private-dir 'doom-user-dir "3.0.0")
  (define-obsolete-variable-alias 'doom-etc-dir 'doom-data-dir "3.0.0"))

;; HACK: Silence obnoxious obsoletion warnings about (if|when)-let in >=31.
;;   These warnings are unhelpful to end-users, and many packages use these
;;   macros, so I silence these warnings to spare users the unactionable spam.
;;   Not to mention, Emacs doesn't respect `warning-suppress-types' when it
;;   comes to obsoletion warnings.
(put 'if-let 'byte-obsolete-info nil)
(put 'when-let 'byte-obsolete-info nil)


;;; Fix $HOME on Windows
;; $HOME isn't normally defined on Windows, but many unix tools expect it.
(when doom--system-windows-p
  (when-let* ((realhome
               (and (null (getenv-internal "HOME"))
                    (getenv "USERPROFILE"))))
    (setenv "HOME" realhome)
    (setq abbreviated-home-dir nil)))


;;; Load Doom's stdlib
(add-to-list 'load-path (file-name-directory load-file-name))
(require 'doom-compat) ; backport niceties from later versions of Emacs
(require 'doom-lib)


;;
;;; Core globals

(defgroup doom nil
  "A development framework for Emacs configurations and Emacs Lisp projects."
  :link '(url-link "https://doomemacs.org")
  :group 'emacs)

(defconst doom-version "3.0.0-pre"
  "Current version of Doom Emacs core.")

;; DEPRECATED: Remove these when the modules are moved out of core.
(defconst doom-modules-version "26.01.0-pre"
  "Current version of Doom Emacs.")

(defvar doom-init-time nil
  "The time it took, in seconds (as a float), for Doom Emacs to start up.")

(defconst doom-profile
  (if-let* ((profile (getenv-internal "DOOMPROFILE")))
      (save-match-data
        (if (string-match "^\\([^@]+\\)@\\(.+\\)$" profile)
            (cons (match-string 1 profile)
                  (match-string 2 profile))
          (cons profile "0")))
    ;; TODO Restore this in 3.0
    ;; (cons "_" "0")
    )
  "The active profile as a cons cell (NAME . VERSION).")

;;; Data directory variables
(defvar doom-emacs-dir user-emacs-directory
  "The path to the currently loaded .emacs.d directory. Must end with a slash.")

(defconst doom-core-dir (file-name-directory load-file-name)
  "The root directory of Doom's core files. Must end with a slash.")

(defvar doom-modules-dir (expand-file-name "modules/" doom-emacs-dir)
  "The root directory for Doom's modules. Must end with a slash.")

(defvar doom-user-dir
  (expand-file-name
   (if-let* ((doomdir (getenv-internal "DOOMDIR")))
       (file-name-as-directory doomdir)
     (or (let ((xdgdir
                (file-name-concat
                 (or (getenv-internal "XDG_CONFIG_HOME")
                     "~/.config")
                 "doom/")))
           (if (file-directory-p xdgdir) xdgdir))
         "~/.doom.d/")))
  "Where your private configuration is placed.

Defaults to ~/.config/doom, ~/.doom.d or the value of the DOOMDIR envvar;
whichever is found first. Must end in a slash.")

;; DEPRECATED: Will be replaced in v3
(defvar doom-module-load-path
  (list (file-name-concat doom-user-dir "modules")
        (file-name-concat doom-emacs-dir "modules"))
  "A list of paths where Doom should search for modules.

Order determines priority (from highest to lowest).

Each entry is a string; an absolute path to the root directory of a module tree.
In other words, they should contain a two-level nested directory structure,
where the module's group and name was deduced from the first and second level of
directories. For example: if $DOOMDIR/modules/ is an entry, a
$DOOMDIR/modules/lang/ruby/ directory represents a ':lang ruby' module.")

;; DEPRECATED: .local will be removed entirely in 3.0
(defvar doom-local-dir
  (if-let* ((localdir (getenv-internal "DOOMLOCALDIR")))
      (expand-file-name (file-name-as-directory localdir))
    (expand-file-name ".local/" doom-emacs-dir))
  "Root directory for local storage.

Use this as a storage location for this system's installation of Doom Emacs.

These files should not be shared across systems. By default, it is used by
`doom-data-dir' and `doom-cache-dir'. Must end with a slash.")

(defvar doom-data-dir
  (if doom-profile
      (if doom--system-windows-p
          (expand-file-name "doomemacs/data/" (getenv-internal "LOCALAPPDATA"))
        (expand-file-name "doom/" (or (getenv-internal "XDG_DATA_HOME") "~/.local/share")))
    ;; DEPRECATED: .local will be removed entirely in 3.0
    (file-name-concat doom-local-dir "etc/"))
  "Where Doom stores its global data files.

Data files contain shared and long-lived data that Doom, Emacs, and their
packages require to function correctly or at all. Deleting them by hand will
cause breakage, and require user intervention (e.g. a `doom sync` or `doom env`)
to restore.

Use this for: server binaries, package source, pulled module libraries,
generated files for profiles, profiles themselves, autoloads/loaddefs, etc.

For profile-local data files, use `doom-profile-data-dir' instead.")

(defvar doom-cache-dir
  (if doom-profile
      (if doom--system-windows-p
          (expand-file-name "doomemacs/cache/" (getenv-internal "LOCALAPPDATA"))
        (expand-file-name "doom/" (or (getenv-internal "XDG_CACHE_HOME") "~/.cache")))
    ;; DEPRECATED: .local will be removed entirely in 3.0
    (file-name-concat doom-local-dir "cache/"))
  "Where Doom stores its global cache files.

Cache files represent unessential data that shouldn't be problematic when
deleted (besides, perhaps, a one-time performance hit), lack portability (and so
shouldn't be copied to other systems/configs), and are regenerated when needed,
without user input (e.g. a `doom sync`).

Some examples: images/data caches, elisp bytecode, natively compiled elisp,
session files, ELPA archives, authinfo files, org-persist, etc.

For profile-local cache files, use `doom-profile-cache-dir' instead.")

(defvar doom-state-dir
  (if doom-profile
      (if doom--system-windows-p
          (expand-file-name "doomemacs/state/" (getenv-internal "LOCALAPPDATA"))
        (expand-file-name "doom/" (or (getenv-internal "XDG_STATE_HOME") "~/.local/state")))
    ;; DEPRECATED: .local will be removed entirely in 3.0
    (file-name-concat doom-local-dir "state/"))
  "Where Doom stores its global state files.

State files contain unessential, non-portable, but persistent data which, if
lost won't cause breakage, but may be inconvenient as they cannot be
automatically regenerated or restored. For example, a recently-opened file list
is not essential, but losing it means losing this record, and restoring it
requires revisiting all those files.

Use this for: history, logs, user-saved data, autosaves/backup files, known
projects, recent files, bookmarks.

For profile-local state files, use `doom-profile-state-dir' instead.")

;;; Profile file/directory variables
(defvar doom-profile-cache-dir
  (file-name-concat doom-cache-dir (car doom-profile))
  "For profile-local cache files under `doom-cache-dir'.")

(defvar doom-profile-data-dir
  (file-name-concat doom-data-dir (car doom-profile))
  "For profile-local data files under `doom-data-dir'.")

(defvar doom-profile-state-dir
  (file-name-concat doom-state-dir (car doom-profile))
  "For profile-local state files under `doom-state-dir'.")

(defconst doom-profile-dir
  (file-name-concat doom-profile-data-dir "@" (cdr doom-profile))
  "Where generated files for the active profile (for Doom's core) are kept.")

;; DEPRECATED: Will be moved to cli/env
(defconst doom-env-file
  (file-name-concat (if doom-profile
                        doom-profile-dir
                      doom-local-dir)
                    "env")
  "The location of your envvar file, generated by `doom env`.

This file contains environment variables scraped from your shell environment,
which is loaded at startup (if it exists). This is helpful if Emacs can't
\(easily) be launched from the correct shell session (particularly for MacOS
users).")

;;; Module file variables
(defvar doom-module-init-file "init.el"
  "The filename for module early initialization config files.

Init files are loaded early, just after Doom core, and before modules' config
files. They are always loaded, even in non-interactive sessions, and before
`doom-before-modules-init-hook'. Related to `doom-module-config-file'.")

(defvar doom-module-config-file "config.el"
  "The filename for module configuration files.

Config files are loaded later, and almost always in interactive sessions. These
run before `doom-after-modules-config-hook' and after `doom-module-init-file'.")

(defvar doom-module-packages-file "packages.el"
  "The filename for the package configuration file.

Package files are read whenever Doom's package manager wants a manifest of all
desired packages. They are rarely read in interactive sessions (unless the user
uses a straight or package.el command directly).")


;;
;;; Startup optimizations

;; Here are Doom's hackiest (and least offensive) startup optimizations. They
;; exploit implementation details and unintended side-effects in Emacs' startup
;; process, and will change often between major Emacs releases. However, I
;; disable them if this is a daemon session (where startup time matters less).
;;
;; Most of these have been tested on Linux and on fairly fast machines (with
;; SSDs), so your mileage may vary depending on hardware and `window-system'.
(unless (daemonp)
  ;; PERF: `file-name-handler-alist' is consulted on each call to `require',
  ;;   `load', or various file/io functions (like `expand-file-name' or
  ;;   `file-remote-p'). You get a noteable boost to startup time by unsetting
  ;;   or simplifying its value.
  (let ((old-value (default-toplevel-value 'file-name-handler-alist)))
    (set-default-toplevel-value
     'file-name-handler-alist
     ;; HACK: The elisp libraries bundled with Emacs are either compressed or
     ;;   not, never both. So if calc-loaddefs.el.gz exists, calc-loaddefs.el
     ;;   won't, and vice versa. This heuristic is used to guess the state of
     ;;   all other built-in (or site); if they're compressed, we must leave the
     ;;   gzip file handler in `file-name-handler-alist' so Emacs knows how to
     ;;   load them. Otherwise, we can omit it (at least during startup) for a
     ;;   boost in package load time.
     (if (eval-when-compile
           (locate-file-internal "calc-loaddefs.el" load-path))
         nil
       (list (rassq 'jka-compr-handler old-value))))
    ;; Remember it so it can be reset where needed.
    (put 'file-name-handler-alist 'initial-value old-value)
    ;; COMPAT: Eventually, Emacs will process any files passed to it via the
    ;;   command line, and will do so *really* early in the startup process.
    ;;   These might contain special file paths like TRAMP paths, so restore
    ;;   `file-name-handler-alist' just for this portion of startup.
    (define-advice command-line-1 (:around (fn args-left) respect-file-handlers)
      (let ((file-name-handler-alist (if args-left old-value file-name-handler-alist)))
        (funcall fn args-left)))
    ;; COMPAT: ...but restore `file-name-handler-alist' later, because it is
    ;;   needed for handling encrypted or compressed files, among other things.
    (add-hook! 'emacs-startup-hook :depth 101
      (defun doom--reset-file-handler-alist-h ()
        (set-default-toplevel-value
         'file-name-handler-alist
         ;; Merge instead of overwrite because there may have been changes to
         ;; `file-name-handler-alist' since startup we want to preserve.
         (delete-dups (append file-name-handler-alist old-value))))))

  (unless noninteractive
    ;; PERF: Resizing the Emacs frame (to accommodate fonts that are smaller or
    ;;   larger than the default system font) can impact startup time
    ;;   dramatically. The larger the delta, the greater the delay. Even trivial
    ;;   deltas can yield up to a ~1000ms loss, depending also on
    ;;   `window-system' (PGTK builds seem least affected and NS/MAC the most).
    (setq frame-inhibit-implied-resize t)

    ;; PERF: A fair bit of startup time goes into initializing the splash and
    ;;   scratch buffers in the typical Emacs session (b/c they activate a
    ;;   non-trivial major mode, generate the splash buffer, and trigger
    ;;   premature frame redraws by writing to *Messages*). These hacks prevent
    ;;   most of this work from happening for some decent savings in startup
    ;;   time. Our dashboard and `doom/open-scratch-buffer' provide a faster
    ;;   (and more useful) alternative anyway.
    (setq inhibit-startup-screen t
          inhibit-startup-echo-area-message user-login-name
          initial-major-mode 'fundamental-mode
          initial-scratch-message nil)
    ;; PERF,UX: Prevent "For information about GNU Emacs..." line in *Messages*.
    (advice-add #'display-startup-echo-area-message :override #'ignore)
    ;; PERF: Suppress the vanilla startup screen completely. We've disabled it
    ;;   with `inhibit-startup-screen', but it would still initialize anyway.
    ;;   This involves file IO and/or bitmap work (depending on the frame type)
    ;;   that we can no-op for a free 50-100ms saving in startup time.
    (advice-add #'display-startup-screen :override #'ignore)

    (unless initial-window-system
      ;; PERF: `tty-run-terminal-initialization' can take 2-3s when starting up
      ;;   TTY Emacs (non-daemon sessions), depending on your TERM, TERMINFO,
      ;;   and TERMCAP, but this work isn't very useful on modern systems (the
      ;;   type I expect Doom's users to be using). The function seems less
      ;;   expensive if run later in the startup process, so I defer it.
      ;; REVIEW: This may no longer be needed in 29+. Needs testing!
      (define-advice tty-run-terminal-initialization (:override (&rest _) defer)
        (advice-remove #'tty-run-terminal-initialization #'tty-run-terminal-initialization@defer)
        (add-hook 'window-setup-hook
                  (doom-partial #'tty-run-terminal-initialization
                                (selected-frame) nil t))))

    ;; These optimizations are brittle, difficult to debug, and obscure other
    ;; issues, so bow out when debug mode is on.
    (unless init-file-debug
      ;; PERF: The mode-line procs a couple dozen times during startup, before
      ;;   the user even sees the first mode-line. This is normally fast, but we
      ;;   can't predict what the user (or packages) will put into the
      ;;   mode-line. Also, mode-line packages have a bad habit of throwing
      ;;   performance to the wind, so best we just disable the mode-line until
      ;;   we can see one.
      (put 'mode-line-format 'initial-value (default-toplevel-value 'mode-line-format))
      (setq-default mode-line-format nil)
      (dolist (buf (buffer-list))
        (with-current-buffer buf (setq mode-line-format nil)))
      ;; PERF,UX: Premature redisplays/redraws can substantially affect startup
      ;;   times and/or flash a white/unstyled Emacs frame during startup, so I
      ;;   try real hard to suppress them until we're sure the session is ready.
      (setq-default inhibit-redisplay t
                    inhibit-message t)
      ;; COMPAT: If the above vars aren't reset, Emacs could appear frozen or
      ;;   garbled after startup (or in case of an startup error).
      (defun doom--reset-inhibited-vars-h ()
        (setq-default inhibit-redisplay nil
                      inhibit-message nil)
        (remove-hook 'post-command-hook #'doom--reset-inhibited-vars-h))
      (add-hook 'post-command-hook #'doom--reset-inhibited-vars-h -100))

    ;; PERF: Doom disables the UI elements by default, so that there's less for
    ;;   the frame to initialize. However, `tool-bar-setup' is still called and
    ;;   it does some non-trivial work to set up the toolbar before we can
    ;;   disable it. To side-step this work, I disable the function and call it
    ;;   later (see `startup--load-user-init-file@undo-hacks').
    (advice-add #'tool-bar-setup :override #'ignore)

    ;; PERF,UX: site-lisp files are often obnoxiously noisy (emitting output
    ;;   that isn't useful to end-users, like load messages, deprecation
    ;;   notices, and linter warnings). Displaying these in the minibuffer
    ;;   causes unnecessary redraws at startup which can impact startup time
    ;;   drastically and cause flashes of white. It also pollutes the logs. I
    ;;   suppress it here and load it myself, later, in a more controlled way
    ;;   (see `doom-initialize').
    (put 'site-run-file 'initial-value site-run-file)
    (setq site-run-file nil)

    (define-advice startup--load-user-init-file (:around (fn &rest args) undo-hacks 95)
      "Undo Doom's startup optimizations to prep for the user's session."
      (unwind-protect (apply fn args)
        ;; Now it's safe to be verbose.
        (setq-default inhibit-message nil)
        ;; COMPAT: Once startup is sufficiently complete, undo our earlier
        ;;   optimizations to reduce the scope of potential edge cases.
        (advice-remove #'tool-bar-setup #'ignore)
        (add-transient-hook! 'tool-bar-mode (tool-bar-setup))
        (unless (default-toplevel-value 'mode-line-format)
          (setq-default mode-line-format (get 'mode-line-format 'initial-value)))))

    ;; PERF: Unset a non-trivial list of command line options that aren't
    ;;   relevant to this session, but `command-line-1' still processes.
    (unless doom--system-macos-p
      (setq command-line-ns-option-alist nil))
    (unless (memq initial-window-system '(x pgtk))
      (setq command-line-x-option-alist nil))))


;;
;;; Reasonable, global defaults

;;; CLI settings
(when noninteractive
  ;; Don't generate superfluous files when writing temp buffers.
  (setq make-backup-files nil)
  ;; Stop user config from interfering with elisp shell scripts.
  (setq enable-dir-local-variables nil)
  ;; Reduce ambiguity, embrace specificity, enjoy predictability.
  (setq case-fold-search nil)
  ;; Don't clog the user's trash with our CLI refuse.
  (setq delete-by-moving-to-trash nil))

;;; Don't litter `doom-emacs-dir'/$HOME
;; HACK: I change `user-emacs-directory' because many packages (even built-in
;;   ones) abuse it to build paths for storage/cache files (instead of correctly
;;   using `locate-user-emacs-file'). This change ensures that said data files
;;   are never saved to the root of your emacs directory *and* saves us the
;;   trouble of setting a million directory/file variables.
(setq user-emacs-directory doom-profile-cache-dir)

;; ...However, this can surprise packages (and users) that read
;; `user-emacs-directory' expecting to find the location of your Emacs config,
;; such as server.el!
(setq server-auth-dir (file-name-concat doom-emacs-dir "server/"))

;; If a packages doesn't use `user-emacs-directory' or `locate-user-emacs-file'
;; to set their file/dir variables, then we need to set them ourselves to avoid
;; littering in ~/.emacs.d/.
(setq desktop-dirname  (file-name-concat doom-profile-state-dir "desktop")
      pcache-directory (file-name-concat doom-profile-cache-dir "pcache/"))

;; Write custom.el settings to $DOOMDIR/custom.el instead of $EMACSDIR/init.el,
;; allowing users to version control them and not interfere with Doom init.
(setq custom-file (file-name-concat doom-user-dir "custom.el"))

(define-advice en/disable-command (:around (fn &rest args) write-to-data-dir)
  "Save safe-local-variables to `custom-file' instead of `user-init-file'.

Otherwise, `en/disable-command' (in novice.el.gz) is hardcoded to write them to
`user-init-file')."
  (let ((user-init-file custom-file))
    (apply fn args)))

;;; Native compilation support (see http://akrl.sdf.org/gccemacs.html)
(when (boundp 'native-comp-eln-load-path)
  ;; Don't store eln files in ~/.emacs.d/eln-cache (where they can easily be
  ;; deleted by 'doom upgrade').
  ;; REVIEW: Advise `startup-redirect-eln-cache' when 28 support is dropped.
  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln/" doom-profile-cache-dir))

  ;; UX: Suppress compiler warnings and don't inundate users with their popups.
  ;;   They are rarely more than warnings, so are safe to ignore.
  (setq native-comp-async-report-warnings-errors init-file-debug
        native-comp-warning-on-missing-source init-file-debug)

  ;; HACK: `native-comp-deferred-compilation-deny-list' is replaced in Emacs 29,
  ;;   and with no deprecation warning. This alias ensures backwards
  ;;   compatibility for packages downstream. I don't mark it obsolete because
  ;;   those warnings should be shown to package devs not end-users.
  (unless (boundp 'native-comp-deferred-compilation-deny-list)
    (defvaralias 'native-comp-deferred-compilation-deny-list 'native-comp-jit-compilation-deny-list))

  ;; UX: By default, native-comp uses half your cores (and tends to saturate
  ;;   them). The sudden (and silent) spike of CPU and memory utilization alarms
  ;;   folks, overheats laptops, and overwhelms less performant systems, so
  ;;   default to 1/4 of cores instead.
  (define-advice comp-effective-async-max-jobs (:before (&rest _) set-default-cpus)
    "Default to 1/4 of cores in interactive sessions and all of them otherwise."
    (and (null comp-num-cpus)
         (zerop native-comp-async-jobs-number)
         (setq comp-num-cpus
               (max 1 (/ (num-processors) (if noninteractive 1 4))))))

  (define-advice comp-run-async-workers (:around (fn &rest args) dont-litter-tmpdir)
    "Normally, native-comp writes a ton to /tmp. This advice redirects this IO
to `doom-profile-cache-dir' instead, so it doesn't OOM tmpfs users and can be
safely cleaned up with 'doom sync' or 'doom gc'."
    (let ((temporary-file-directory (expand-file-name "comp/" doom-profile-cache-dir)))
      (make-directory temporary-file-directory t)
      (apply fn args)))

  (with-eval-after-load 'comp
    ;; HACK: On Emacs 30.0.92, `native-comp-jit-compilation-deny-list' was moved
    ;;   to comp-run. See emacsmirror/emacs@e6a955d24268. Doom forces straight
    ;;   to consult this variable when building packages.
    (require 'comp-run nil t)
    ;; HACK: Disable native-compilation for some troublesome packages
    (mapc (doom-partial #'add-to-list 'native-comp-deferred-compilation-deny-list)
          (list "/seq-tests\\.el\\'"
                "/emacs-jupyter.*\\.el\\'"
                "/evil-collection-vterm\\.el\\'"
                "/vterm\\.el\\'"
                "/with-editor\\.el\\'"))))


;;; Reduce unnecessary/unactionable warnings/logs
;; Disable warnings from the legacy advice API. They aren't actionable or
;; useful, and often come from third party packages.
(setq ad-redefinition-action 'accept)

;; Ignore warnings about "existing variables being aliased". Otherwise the user
;; gets very intrusive popup warnings about our (intentional) uses of
;; defvaralias, which are done because ensuring aliases are created before
;; packages are loaded is an unneeded and unhelpful maintenance burden. Emacs
;; still aliases them fine regardless.
(setq warning-suppress-types '((defvaralias) (lexical-binding)))

;; As some point in 31+, Emacs began spamming the user with warnings about
;; missing `lexical-binding' cookies in elisp files that you are unlikely to
;; have any direct control over (e.g. package files, data lisp files, and elisp
;; shell scripts). This shuts it up.
(setq warning-inhibit-types '((files missing-lexbind-cookie)))

;; Reduce debug output unless we've asked for it.
(setq debug-on-error init-file-debug
      jka-compr-verbose init-file-debug)

;;; Stricter security defaults
;; Emacs is essentially one huge security vulnerability, what with all the
;; dependencies it pulls in from all corners of the globe. Let's try to be a
;; *little* more discerning.
(setq gnutls-verify-error noninteractive
      gnutls-algorithm-priority
      (when (boundp 'libgnutls-version)
        (concat "SECURE128:+SECURE192:-VERS-ALL"
                (if (and (not doom--system-windows-p)
                         (>= libgnutls-version 30605))
                    ":+VERS-TLS1.3")
                ":+VERS-TLS1.2"))
      ;; `gnutls-min-prime-bits' is set based on recommendations from
      ;; https://www.keylength.com/en/4/
      gnutls-min-prime-bits 3072
      tls-checktrust gnutls-verify-error
      ;; Emacs is built with gnutls.el by default, so `tls-program' won't
      ;; typically be used, but in the odd case that it does, we ensure a more
      ;; secure default for it (falling back to `openssl' if absolutely
      ;; necessary). See https://redd.it/8sykl1 for details.
      tls-program '("openssl s_client -connect %h:%p -CAfile %t -nbio -no_ssl3 -no_tls1 -no_tls1_1 -ign_eof"
                    "gnutls-cli -p %p --dh-bits=3072 --ocsp --x509cafile=%t \
--strict-tofu --priority='SECURE192:+SECURE128:-VERS-ALL:+VERS-TLS1.2:+VERS-TLS1.3' %h"
                    ;; compatibility fallbacks
                    "gnutls-cli -p %p %h"))


;;; Package managers
;; Since Emacs 27, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. Doom handles package initialization, so
;; we must prevent Emacs from doing it again.
(setq package-enable-at-startup nil)

;; Ensure that, if the user does want package.el, it is configured correctly.
;; You really shouldn't be using it, though...
(with-eval-after-load 'package
  (setq package-user-dir (file-name-concat doom-local-dir "elpa/")
        package-gnupghome-dir (expand-file-name "gpg" package-user-dir))
  (let ((s (if gnutls-verify-error "s" "")))
    (cl-callf2 append
        ;; I omit Marmalade because its packages are manually submitted rather
        ;; than pulled, and so often out of date.
        `(("melpa" . ,(format "http%s://melpa.org/packages/" s))
          ("org"   . ,(format "http%s://orgmode.org/elpa/"   s)))
        package-archives))

  ;; Refresh package.el the first time you call `package-install', so it's still
  ;; trivially usable. Remember to run 'doom sync' to purge them; they can
  ;; conflict with packages installed via straight!
  (add-transient-hook! 'package-install (package-refresh-contents)))

;; DEPRECATED: Interactive sessions won't be able to interact with Straight (or
;;   Elpaca) in the future, so this is temporary.
(with-eval-after-load 'straight
  (require 'doom-straight)
  (doom-initialize-packages))


;;
;;; Custom hooks

(defcustom doom-before-init-hook ()
  "A hook run after Doom's core has initialized; before user configuration.

This is triggered right before $DOOMDIR/init.el is loaded, in the context of
early-init.el. Use this for configuration at the latest opportunity before the
session becomes unpredictably complicated by user config, packages, etc. This
runs in both interactive and non-interactive contexts, so guard hooks
appropriately against `noninteractive' or the `cli' context (see
`doom-context').

In contrast, `before-init-hook' is run just after $DOOMDIR/init.el is loaded,
but long before your modules and $DOOMDIR/config.el are loaded."
  :group 'doom
  :type 'hook)

(defcustom doom-after-init-hook ()
  "A hook run once Doom's core and modules, and the user's config are loaded.

This triggers at the absolute latest point in the eager startup process, and
runs in both interactive and non-interactive sessions, so guard hooks
appropriately against `noninteractive' or the `cli' context."
  :group 'doom
  :type 'hook)

(defcustom doom-before-modules-init-hook nil
  "Hooks run before module init.el files are loaded."
  :group 'doom
  :type 'hook)

(defcustom doom-after-modules-init-hook nil
  "Hooks run after module init.el files are loaded."
  :group 'doom
  :type 'hook)

(defcustom doom-before-modules-config-hook nil
  "Hooks run before module config.el files are loaded."
  :group 'doom
  :type 'hook)

(defcustom doom-after-modules-config-hook nil
  "Hooks run after module config.el files are loaded (but before the user's)."
  :group 'doom
  :type 'hook)


;;
;;; Initializers

(defun doom-initialize (&optional interactive?)
  "Bootstrap the Doom session ahead."
  (when (doom-context-push 'startup)
    (when (daemonp)
      (message "Starting Doom Emacs in daemon mode...")
      (unless doom-inhibit-log
        (add-hook! 'doom-after-init-hook :depth 106
          (unless doom-inhibit-log
            (setq doom-inhibit-log (not (or noninteractive init-file-debug))))
          (message "Disabling verbose mode. Have fun!"))
        (add-hook! 'kill-emacs-hook :depth 110
          (message "Killing Emacs. Sayonara!"))))

    (if interactive?
        (when (doom-context-push 'emacs)
          (add-hook 'doom-after-init-hook #'doom-load-packages-incrementally-h 100)
          (add-hook 'doom-after-init-hook #'doom-display-benchmark-h 110)
          (doom-run-hook-on 'doom-first-buffer-hook '(find-file-hook doom-switch-buffer-hook))
          (doom-run-hook-on 'doom-first-file-hook   '(find-file-hook dired-initial-position-hook))
          (doom-run-hook-on 'doom-first-input-hook  '(pre-command-hook))

          ;; If the user's already opened something (e.g. with command-line
          ;; arguments), then we should assume nothing about the user's
          ;; intentions and simply treat this session as fully initialized.
          (add-hook! 'doom-after-init-hook :depth 100
            (defun doom-run-first-hooks-if-files-open-h ()
              (when file-name-history
                (doom-run-hooks 'doom-first-file-hook 'doom-first-buffer-hook))))

          ;; These fire `MAJOR-MODE-local-vars-hook' hooks, which is a Doomism.
          ;; See the `MODE-local-vars-hook' section above.
          (add-hook 'after-change-major-mode-hook #'doom-run-local-var-hooks-h 100)
          (add-hook 'hack-local-variables-hook #'doom-run-local-var-hooks-h)

          ;; This is the absolute latest a hook can run in Emacs' startup
          ;; process.
          (advice-add #'command-line-1 :after #'doom-finalize)

          (require 'doom-start)
          (let ((init-file (doom-profile-init-file doom-profile)))
            (or (doom-load init-file t)
                (signal 'doom-nosync-error (list init-file)))))

      (when (doom-context-push 'cli)
        ;; REVIEW: Remove later. The endpoints should be responsibile for
        ;;   ensuring they exist. For now, they exist to quell file errors.
        (with-file-modes #o700
          (mapc (doom-rpartial #'make-directory 'parents)
                (list doom-local-dir
                      doom-data-dir
                      doom-cache-dir
                      doom-state-dir)))

        (doom-require 'doom-lib 'debug)
        (if init-file-debug (doom-debug-mode +1))

        ;; Then load the rest of Doom's libs eagerly, since autoloads may not
        ;; be generated/loaded yet.
        (require 'seq)
        (require 'map)
        (mapc (doom-partial #'doom-require 'doom-lib)
              '(process
                system
                git
                plist
                files
                print
                autoloads
                profiles
                modules
                packages))

        ;; Ensure the CLI framework is ready.
        (require 'doom-cli)
        (add-hook 'doom-cli-initialize-hook #'doom-finalize)))

    ;; HACK: I suppress loading of site files here to load them manually later.
    ;;   Why? To suppress the otherwise unavoidable output they commonly produce
    ;;   (like deprecation notices, file-loaded messages, and linter warnings).
    ;;   This output pollutes Emacs' log and the output of doom's CLI (or
    ;;   scripts derived from it) with potentially confusing or alarming -- but
    ;;   always unimportant and rarely actionable -- information to the user. To
    ;;   see that output, turn on debug mode!
    (let ((site-loader
           (lambda ()
             (quiet!!
               (unless interactive?
                 (require 'cl nil t))  ; "Package cl is deprecated"
               (unless site-run-file
                 (when-let* ((site-file (get 'site-run-file 'initial-value)))
                   (let ((inhibit-startup-screen inhibit-startup-screen))
                     (setq site-run-file site-file)
                     (load site-run-file t))))))))
      (if interactive?
          (define-advice startup--load-user-init-file (:before (&rest _) load-site-files 100)
            (funcall site-loader))
        (funcall site-loader)))

    ;; A last ditch opportunity to undo hacks or do extra configuration before
    ;; the session is complicated by user config and packages.
    (doom-run-hooks 'doom-before-init-hook)

    ;; HACK: Ensure OS checks are as fast as possible (given their ubiquity).
    (setq features (cons :system (delq :system features)))

    ;; Remember these variables' initial values, so they can be safely reset
    ;; later (e.g. by `doom/reload'), or compared against for change heuristics.
    (dolist (var '(exec-path load-path process-environment))
      (put var 'initial-value (default-toplevel-value var)))

    t))

(defun doom-finalize (&rest _)
  "Finalize the current Doom session, marking the end of its startup process.

Triggers `doom-after-init-hook' and sets `doom-init-time.'"
  (when (doom-context-pop 'startup)
    (setq doom-init-time (float-time (time-subtract (current-time) before-init-time)))
    (doom-run-hooks 'doom-after-init-hook)

    ;; If `gc-cons-threshold' hasn't been reset at this point, we reset it by
    ;; force (without overwriting `gcmh' or the user's config). If this isn't
    ;; done, this session will be prone to freezing and crashes. This also
    ;; handles the case where the user has `gcmh' disabled.
    (when (eq (default-value 'gc-cons-threshold) most-positive-fixnum)
      (setq-default gc-cons-threshold (* 16 1024 1024)))
    t))

(provide 'doom)
;;; doom.el ends here
