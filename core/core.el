;;; core.el --- the heart of the beast -*- lexical-binding: t; -*-
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
;;   $EMACSDIR/early-init.el
;;   $EMACSDIR/core/core.el
;;   $DOOMDIR/init.el
;;   {$DOOMDIR,~/.emacs.d}/modules/*/*/init.el
;;   `doom-before-init-modules-hook'
;;   {$DOOMDIR,~/.emacs.d}/modules/*/*/config.el
;;   `doom-init-modules-hook'
;;   $DOOMDIR/config.el
;;   `doom-after-init-modules-hook'
;;   `after-init-hook'
;;   `emacs-startup-hook'
;;   `doom-init-ui-hook'
;;   `window-setup-hook'
;;
;;; Code:

(when (< emacs-major-version 27)
  (error "Detected Emacs %s. Minimum supported version is 27.1."
         emacs-version))

;; Remember these variables' initial values, so we can safely reset them at a
;; later time, or consult them without fear of contamination.
(dolist (var '(exec-path load-path process-environment))
  (unless (get var 'initial-value)
    (put var 'initial-value (default-value var))))

;; Prevent unwanted runtime compilation for gccemacs (native-comp) users;
;; packages are compiled ahead-of-time when they are installed and site files
;; are compiled when gccemacs is installed.
(setq native-comp-deferred-compilation nil)

;; Since Emacs 27, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. Doom handles package initialization, so
;; we must prevent Emacs from doing it again.
(setq package-enable-at-startup nil)

;; Just the... bear necessities~
(require 'cl-lib)
(require 'subr-x)


;;
;;; Global constants

;; Emacs features
(defconst EMACS28+    (> emacs-major-version 27))
(defconst EMACS29+    (> emacs-major-version 28))
(defconst MODULES     (bound-and-true-p module-file-suffix))
(defconst NATIVECOMP  (if (fboundp 'native-comp-available-p) (native-comp-available-p)))

;; Operating system
(defconst IS-MAC      (eq system-type 'darwin))
(defconst IS-LINUX    (eq system-type 'gnu/linux))
(defconst IS-WINDOWS  (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD      (or IS-MAC (eq system-type 'berkeley-unix)))


;;
;;; Cross-platform fixes

(when IS-WINDOWS
  (when-let (realhome
             ;; Fix HOME on Windows, where it's not normally defined (though
             ;; many unix tools expect it).
             (and (null (getenv-internal "HOME"))
                  (getenv "USERPROFILE")))
    (setenv "HOME" realhome)
    (setq abbreviated-home-dir nil)))


;;
;;; Core variables

(defgroup doom nil
  "An Emacs framework for the stubborn martian hacker."
  :link '(url-link "https://doomemacs.org"))

(defconst doom-version "3.0.0-dev"
  "Current version of Doom Emacs core.")

(defconst doom-modules-version "22.07.0-dev"
  "Current version of Doom Emacs.")


;;
;;; Directory variables

(defconst doom-emacs-dir user-emacs-directory
  "The path to the currently loaded .emacs.d directory. Must end with a slash.")

(defconst doom-core-dir (concat doom-emacs-dir "core/")
  "The root directory of Doom's core files. Must end with a slash.")

(defconst doom-modules-dir (concat doom-emacs-dir "modules/")
  "The root directory for Doom's modules. Must end with a slash.")

(defconst doom-local-dir
  (if-let (localdir (getenv-internal "DOOMLOCALDIR"))
      (expand-file-name (file-name-as-directory localdir))
    (concat doom-emacs-dir ".local/"))
  "Root directory for local storage.

Use this as a storage location for this system's installation of Doom Emacs.

These files should not be shared across systems. By default, it is used by
`doom-etc-dir' and `doom-cache-dir'. Must end with a slash.")

;; DEPRECATED
(defconst doom-etc-dir (concat doom-local-dir "etc/")
  "Directory for non-volatile local storage.

Use this for files that don't change much, like server binaries, external
dependencies or long-term shared data. Must end with a slash.")

;; DEPRECATED
(defconst doom-cache-dir (concat doom-local-dir "cache/")
  "Directory for volatile local storage.

Use this for files that change often, like cache files. Must end with a slash.")

(defconst doom-docs-dir (concat doom-emacs-dir "docs/")
  "Where Doom's documentation files are stored. Must end with a slash.")

(defconst doom-private-dir
  (if-let (doomdir (getenv-internal "DOOMDIR"))
      (expand-file-name (file-name-as-directory doomdir))
    (or (let ((xdgdir
               (expand-file-name "doom/"
                                 (or (getenv-internal "XDG_CONFIG_HOME")
                                     "~/.config"))))
          (if (file-directory-p xdgdir) xdgdir))
        "~/.doom.d/"))
  "Where your private configuration is placed.

Defaults to ~/.config/doom, ~/.doom.d or the value of the DOOMDIR envvar;
whichever is found first. Must end in a slash.")

;; DEPRECATED
(defconst doom-autoloads-file
  (concat doom-local-dir "autoloads." emacs-version ".el")
  "Where `doom-reload-core-autoloads' stores its core autoloads.

This file is responsible for informing Emacs where to find all of Doom's
autoloaded core functions (in core/autoload/*.el).")

(defconst doom-env-file (concat doom-local-dir "env")
  "The location of your envvar file, generated by `doom env`.

This file contains environment variables scraped from your shell environment,
which is loaded at startup (if it exists). This is helpful if Emacs can't
\(easily) be launched from the correct shell session (particularly for MacOS
users).")


;;
;;; Custom error types

(define-error 'doom-error "Error in Doom Emacs core")
(define-error 'doom-hook-error "Error in a Doom startup hook" 'doom-error)
(define-error 'doom-autoload-error "Error in Doom's autoloads file" 'doom-error)
(define-error 'doom-module-error "Error in a Doom module" 'doom-error)
(define-error 'doom-private-error "Error in private config" 'doom-error)
(define-error 'doom-package-error "Error with packages" 'doom-error)


;;
;;; Custom hooks

(defvar doom-first-input-hook nil
  "Transient hooks run before the first user input.")
(put 'doom-first-input-hook 'permanent-local t)

(defvar doom-first-file-hook nil
  "Transient hooks run before the first interactively opened file.")
(put 'doom-first-file-hook 'permanent-local t)

(defvar doom-first-buffer-hook nil
  "Transient hooks run before the first interactively opened buffer.")
(put 'doom-first-buffer-hook 'permanent-local t)

(defvar doom-after-reload-hook nil
  "A list of hooks to run after `doom/reload' has reloaded Doom.")

(defvar doom-before-reload-hook nil
  "A list of hooks to run before `doom/reload' has reloaded Doom.")


;;
;;; Native Compilation support (http://akrl.sdf.org/gccemacs.html)

(when NATIVECOMP
  ;; Don't store eln files in ~/.emacs.d/eln-cache (where they can easily be
  ;; deleted by 'doom upgrade').
  (add-to-list 'native-comp-eln-load-path (concat doom-cache-dir "eln/"))

  (with-eval-after-load 'comp
    ;; HACK Disable native-compilation for some troublesome packages
    (mapc (apply-partially #'add-to-list 'native-comp-deferred-compilation-deny-list)
          (let ((local-dir-re (concat "\\`" (regexp-quote doom-local-dir))))
            (list (concat local-dir-re ".*/emacs-jupyter.*\\.el\\'")
                  (concat local-dir-re ".*/evil-collection-vterm\\.el\\'")
                  (concat local-dir-re ".*/with-editor\\.el\\'"))))))


;;
;;; Don't litter `doom-emacs-dir'

;; I change `user-emacs-directory' because many packages (even built-in ones)
;; abuse it to build paths for storage/cache files (instead of correctly using
;; `locate-user-emacs-file'). This change ensures that said data files are never
;; saved to the root of your emacs directory *and* saves us the trouble setting
;; a million directory/file variables.
(setq user-emacs-directory doom-cache-dir)

;; ...However, this may surprise packages (and users) that read
;; `user-emacs-directory' expecting to find the location of your Emacs config,
;; such as server.el!
(setq server-auth-dir (expand-file-name "server/" doom-emacs-dir))

;; Packages with file/dir settings that don't use `user-emacs-directory' or
;; `locate-user-emacs-file' to initialize will need to set explicitly, to stop
;; them from littering in ~/.emacs.d/.
(setq desktop-dirname  (expand-file-name "desktop" doom-cache-dir)
      pcache-directory (expand-file-name "pcache/" doom-cache-dir))

;; Allow the user to store custom.el-saved settings and themes in their Doom
;; config (e.g. ~/.doom.d/).
(setq custom-file (expand-file-name "custom.el" doom-private-dir))

(define-advice en/disable-command (:around (fn &rest args) write-to-data-dir)
  "Write saved safe-local-variables to `custom-file' instead.

Otherwise, `en/disable-command' (in novice.el.gz) is hardcoded to write them to
`user-init-file')."
  (let ((user-init-file custom-file))
    (apply fn args)))


;;
;;; Bootstrap

;; Ensure Doom's core libraries are visible for loading
(add-to-list 'load-path doom-core-dir)

;; ...then load *the* one
(require 'core-lib)


;;
;;; Runtime/startup optimizations

;; A second, case-insensitive pass over `auto-mode-alist' is time wasted and
;; indicates misconfiguration.
(setq auto-mode-case-fold nil)

;; Disable bidirectional text scanning for a modest performance boost. I've set
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

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).
(setq frame-inhibit-implied-resize t)

;; The GC introduces annoying pauses and stuttering into our Emacs experience,
;; so we use `gcmh' to stave off the GC while we're using Emacs, and provoke it
;; when it's idle. However, if the idle delay is too long, we run the risk of
;; runaway memory usage in busy sessions. If it's too low, then we may as well
;; not be using gcmh at all.
(setq gcmh-idle-delay 'auto  ; default is 15s
      gcmh-auto-idle-delay-factor 10
      gcmh-high-cons-threshold (* 16 1024 1024))  ; 16mb

;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
(setq idle-update-delay 1.0)  ; default is 0.5

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
;; hasn't been determined, but do it anyway, just in case. This increases memory
;; usage, however!
(setq inhibit-compacting-font-caches t)

;; PGTK builds only: this timeout adds latency to frame operations, like
;; `make-frame-invisible', which are frequently called without a guard because
;; it's inexpensive in non-PGTK builds. Lowering the timeout from the default
;; 0.1 should make childframes and packages that manipulate them (like `lsp-ui',
;; `company-box', and `posframe') feel much snappier. See emacs-lsp/lsp-ui#613.
(setq pgtk-wait-for-event-timeout 0.001)

;; Increase how much is read from processes in a single chunk (default is 4kb).
;; This is further increased elsewhere, where needed (like our LSP module).
(setq read-process-output-max (* 64 1024))  ; 64kb

;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification while
;; receiving input, which should help a little with scrolling performance.
(setq redisplay-skip-fontification-on-input t)

;; Performance on Windows is considerably worse than elsewhere. We'll need
;; everything we can get.
(when (boundp 'w32-get-true-file-attributes)
  (setq w32-get-true-file-attributes nil   ; decrease file IO workload
        w32-pipe-read-delay 0              ; faster IPC
        w32-pipe-buffer-size (* 64 1024))) ; read more at a time (was 4K)

;; Remove command line options that aren't relevant to our current OS; means
;; slightly less to process at startup.
(unless IS-MAC    (setq command-line-ns-option-alist nil))
(unless IS-LINUX  (setq command-line-x-option-alist nil))

;; HACK `tty-run-terminal-initialization' is *tremendously* slow for some
;;      reason; inexplicably doubling startup time for terminal Emacs. Keeping
;;      it disabled will have nasty side-effects, so we simply delay it instead,
;;      and invoke it later, at which point it runs quickly; how mysterious!
(unless (daemonp)
  (advice-add #'tty-run-terminal-initialization :override #'ignore)
  (add-hook! 'window-setup-hook
    (defun doom-init-tty-h ()
      (advice-remove #'tty-run-terminal-initialization #'ignore)
      (tty-run-terminal-initialization (selected-frame) nil t))))

;; Shave seconds off startup time by starting the scratch buffer in
;; `fundamental-mode', rather than, say, `org-mode' or `text-mode', which pull
;; in a ton of packages. `doom/open-scratch-buffer' provides a better scratch
;; buffer anyway.
(setq initial-major-mode 'fundamental-mode
      initial-scratch-message nil)


;;
;;; Reasonable, global defaults

;; Contrary to what many Emacs users have in their configs, you don't need more
;; than this to make UTF-8 the default coding system:
(set-language-environment "UTF-8")
;; ...but `set-language-environment' also sets `default-input-method', which is
;; a step too opinionated.
(setq default-input-method nil)
;; ...And the clipboard on Windows could be in a wider encoding (UTF-16), so
;; leave Emacs to its own devices.
(unless IS-WINDOWS
  (setq selection-coding-system 'utf-8))

;; Disable warnings from the legacy advice API. They aren't actionable or
;; useful, and often come from third party packages.
(setq ad-redefinition-action 'accept)

;; Reduce debug output unless we've asked for it.
(setq debug-on-error init-file-debug
      jka-compr-verbose init-file-debug)

;; Emacs stores `authinfo' in $HOME and in plain-text. Let's not do that, mkay?
;; This file stores usernames, passwords, and other treasures for the aspiring
;; malicious third party. However, for this to work you need a GPG setup.
(setq auth-sources (list (concat doom-etc-dir "authinfo.gpg")
                         "~/.authinfo.gpg"))

;; Get rid of "For information about GNU Emacs..." message at startup, unless
;; we're in a daemon session where it'll say "Starting Emacs daemon." instead,
;; which isn't so bad.
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

;; Reduce *Message* noise at startup. An empty scratch buffer (or the dashboard)
;; is more than enough.
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t)

;; Emacs is essentially one huge security vulnerability, what with all the
;; dependencies it pulls in from all corners of the globe. Let's try to be a
;; *little* more discerning.
(setq gnutls-verify-error noninteractive
      gnutls-algorithm-priority
      (when (boundp 'libgnutls-version)
        (concat "SECURE128:+SECURE192:-VERS-ALL"
                (if (and (not IS-WINDOWS)
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

(provide 'core)
;;; core.el ends here
