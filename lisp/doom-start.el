;;; lisp/doom-start.el --- bootstraps interactive sessions -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;
;;; Custom hooks

(defcustom doom-first-input-hook ()
  "Transient hooks run before the first user input."
  :type 'hook
  :local 'permanent-local
  :group 'doom)

(defcustom doom-first-file-hook ()
  "Transient hooks run before the first interactively opened file."
  :type 'hook
  :local 'permanent-local
  :group 'doom)

(defcustom doom-first-buffer-hook ()
  "Transient hooks run before the first interactively opened buffer."
  :type 'hook
  :local 'permanent-local
  :group 'doom)


;;
;;; Reasonable defaults for interactive sessions

;;; Runtime optimizations
;; PERF: A second, case-insensitive pass over `auto-mode-alist' is time wasted.
(setq auto-mode-case-fold nil)

;; PERF: Disable bidirectional text scanning for a modest performance boost.
;;   I've set this to `nil' in the past, but the `bidi-display-reordering's docs
;;   say that is an undefined state and suggest this to be just as good:
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; PERF: Disabling BPA makes redisplay faster, but might produce incorrect
;;   reordering of bidirectional text with embedded parentheses (and other
;;   bracket characters whose 'paired-bracket' Unicode property is non-nil).
(setq bidi-inhibit-bpa t)  ; Emacs 27+ only

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
;; hasn't been determined, but do it anyway, just in case. This increases memory
;; usage, however!
(setq inhibit-compacting-font-caches t)

;; PGTK builds only: there's a timeout that adds latency to frame operations,
;; like `make-frame-invisible', which Emacs frequently calls without a guard
;; because it's inexpensive in non-PGTK builds. Lowering the timeout from the
;; default 0.1 should make childframes and packages that manipulate them (like
;; `lsp-ui', `company-box', and `posframe') feel much snappier. See
;; emacs-lsp/lsp-ui#613.
(when (boundp 'pgtk-wait-for-event-timeout)
  (setq pgtk-wait-for-event-timeout 0.001))

;; Increase how much is read from processes in a single chunk (default is 4kb).
;; This is further increased elsewhere, where needed (like our LSP module).
(setq read-process-output-max (* 64 1024))  ; 64kb

;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification while
;; receiving input, which should help a little with scrolling performance.
(setq redisplay-skip-fontification-on-input t)

;; Performance on Windows is considerably worse than elsewhere. We'll need
;; everything we can get.
(when (boundp 'w32-get-true-file-attributes)
  (setq w32-get-true-file-attributes nil    ; decrease file IO workload
        w32-pipe-read-delay 0               ; faster IPC
        w32-pipe-buffer-size (* 64 1024)))  ; read more at a time (was 4K)

;; The GC introduces annoying pauses and stuttering into our Emacs experience,
;; so we use `gcmh' to stave off the GC while we're using Emacs, and provoke it
;; when it's idle. However, if the idle delay is too long, we run the risk of
;; runaway memory usage in busy sessions. And if it's too low, then we may as
;; well not be using gcmh at all.
(setq gcmh-idle-delay 'auto  ; default is 15s
      gcmh-auto-idle-delay-factor 10
      gcmh-high-cons-threshold (* 16 1024 1024))  ; 16mb
(add-hook 'doom-first-buffer-hook #'gcmh-mode)


;;; Disable UI elements early
;; PERF,UI: Doom strives to be keyboard-centric, so I consider these UI elements
;;   clutter. Initializing them also costs a morsel of startup time. What's
;;   more, the menu bar exposes functionality that Doom doesn't endorse or
;;   police. Perhaps one day Doom will support these, but today is not that day.
;;   By disabling them early, we save Emacs some time.

;; HACK: I intentionally avoid calling `menu-bar-mode', `tool-bar-mode', and
;;   `scroll-bar-mode' because their manipulation of frame parameters can
;;   trigger/queue a superfluous (and expensive, depending on the window system)
;;   frame redraw at startup. The variables must be set to `nil' as well so
;;   users don't have to call the functions twice to re-enable them.
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;; HACK: The menu-bar needs special treatment on MacOS. On Linux and Windows
;;   (and TTY frames in MacOS), the menu-bar takes up valuable in-frame real
;;   estate -- so we disable it -- but on MacOS (GUI frames only) the menu bar
;;   lives outside of the frame, on the MacOS menu bar, which is acceptable, but
;;   disabling Emacs' menu-bar also makes MacOS treat Emacs GUI frames like
;;   non-application windows (e.g. it won't capture focus on activation, among
;;   other things), so the menu-bar should be preserved there.
(when doom--system-macos-p
  ;; NOTE: Don't try to undo the hack below, as it may change without warning.
  ;;   Instead, toggle `menu-bar-mode' (or put it on a hook) as normal. This
  ;;   hack will always try to respect the state of `menu-bar-mode'.
  (setcdr (assq 'menu-bar-lines default-frame-alist) 'tty)
  (add-hook! 'after-make-frame-functions
    (defun doom--init-menu-bar-on-macos-h (&optional frame)
      (if (eq (frame-parameter frame 'menu-bar-lines) 'tty)
          (set-frame-parameter frame 'menu-bar-lines
                               (if (display-graphic-p frame) 1 0))))))

;;; Encodings
;; Contrary to what many Emacs users have in their configs, you don't need more
;; than this to make UTF-8 the default coding system:
(set-language-environment "UTF-8")
;; ...but `set-language-environment' also sets `default-input-method', which is
;; a step too opinionated.
(setq default-input-method nil)
;; ...And the clipboard on Windows is often a wider encoding (UTF-16), so leave
;; Emacs to its own devices there.
(unless doom--system-windows-p
  (setq selection-coding-system 'utf-8))


;;
;;; MODE-local-vars-hook

;; File+dir local variables are initialized after the major mode and its hooks
;; have run. If you want hook functions to be aware of these customizations, add
;; them to MODE-local-vars-hook instead.
(defvar doom-inhibit-local-var-hooks nil)

(defun doom-run-local-var-hooks-h ()
  "Run MODE-local-vars-hook after local variables are initialized."
  (unless (or doom-inhibit-local-var-hooks
              delay-mode-hooks
              ;; Don't trigger local-vars hooks in temporary (internal) buffers
              (string-prefix-p
               " " (buffer-name (or (buffer-base-buffer)
                                    (current-buffer)))))
    (setq-local doom-inhibit-local-var-hooks t)
    (doom-run-hooks (intern-soft (format "%s-local-vars-hook" major-mode)))))

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
here may cause noticeable pauses, so it's recommended you break them up into
sub-packages. For example, `org' is comprised of many packages, and might be
broken up into:

  (doom-load-packages-incrementally
   '(calendar find-func format-spec org-macs org-compat
     org-faces org-entities org-list org-pcomplete org-src
     org-footnote org-macro ob org org-clock org-agenda
     org-capture))

This is already done by the lang/org module, however.

If you want to disable incremental loading altogether, either remove
`doom-load-packages-incrementally-h' from `doom-after-init-hook' or set
`doom-incremental-first-idle-timer' to nil. Incremental loading does not occur
in daemon sessions (they are loaded immediately at startup).")

(defvar doom-incremental-first-idle-timer (if (daemonp) 0 2.0)
  "How long (in idle seconds) until incremental loading starts.

Set this to nil to disable incremental loading at startup.
Set this to 0 to load all incrementally deferred packages immediately at
`doom-after-init-hook'.")

(defvar doom-incremental-idle-timer 0.75
  "How long (in idle seconds) in between incrementally loading packages.")

(defun doom-load-packages-incrementally (packages &optional now)
  "Registers PACKAGES to be loaded incrementally.

If NOW is non-nil, PACKAGES will be marked for incremental loading next time
Emacs is idle for `doom-incremental-first-idle-timer' seconds (falls back to
`doom-incremental-idle-timer'), then in `doom-incremental-idle-timer' intervals
afterwards."
  (let* ((gc-cons-threshold most-positive-fixnum)
         (first-idle-timer (or doom-incremental-first-idle-timer
                               doom-incremental-idle-timer)))
    (if (not now)
        (cl-callf append doom-incremental-packages packages)
      (while packages
        (let ((req (pop packages))
              idle-time)
          (if (featurep req)
              (doom-log "start:iloader: Already loaded %s (%d left)" req (length packages))
            (condition-case-unless-debug e
                (and
                 (or (null (setq idle-time (current-idle-time)))
                     (< (float-time idle-time) first-idle-timer)
                     (not
                      (while-no-input
                        (doom-log "start:iloader: Loading %s (%d left)" req (length packages))
                        ;; If `default-directory' doesn't exist or is
                        ;; unreadable, Emacs throws file errors.
                        (let ((default-directory doom-emacs-dir)
                              (inhibit-message t)
                              (file-name-handler-alist
                               (list (rassq 'jka-compr-handler file-name-handler-alist))))
                          (require req nil t)
                          t))))
                 (push req packages))
              (error
               (message "Error: failed to incrementally load %S because: %s" req e)
               (setq packages nil)))
            (if (null packages)
                (doom-log "start:iloader: Finished!")
              (run-at-time (if idle-time
                               doom-incremental-idle-timer
                             first-idle-timer)
                           nil #'doom-load-packages-incrementally
                           packages t)
              (setq packages nil))))))))

(defun doom-load-packages-incrementally-h ()
  "Begin incrementally loading packages in `doom-incremental-packages'.

If this is a daemon session, load them all immediately instead."
  (when (numberp doom-incremental-first-idle-timer)
    (if (zerop doom-incremental-first-idle-timer)
        (mapc #'require (cdr doom-incremental-packages))
      (run-with-idle-timer doom-incremental-first-idle-timer
                           nil #'doom-load-packages-incrementally
                           (cdr doom-incremental-packages) t))))


;;
;;; Benchmark

(defun doom-display-benchmark-h (&optional return-p)
  "Display a benchmark including number of packages and modules loaded.

If RETURN-P, return the message as a string instead of displaying it."
  (funcall (if return-p #'format #'message)
           "Doom loaded %d packages across %d modules in %.03fs"
           (- (length load-path) (length (get 'load-path 'initial-value)))
           (if doom-modules (hash-table-count doom-modules) -1)
           doom-init-time))


;;
;;; Entry point
;; HACK: This advice hijacks Emacs' initfile loader to accomplish the following:
;;
;;   1. Load the profile init file directory (generated on `doom sync`)
;;   2. Ignore initfiles we don't care about (like $EMACSDIR/init.el, ~/.emacs,
;;      and ~/_emacs) -- and spare us the IO of searching for them, and allows
;;      savvy hackers to use $EMACSDIR as their $DOOMDIR, if they wanted.
;;   3. Cut down on unnecessary logic in Emacs' bootstrapper.
;;   4. TODO Offer a more user-friendly error state/screen, especially for
;;      errors emitted from Doom's core or the user's config.
(define-advice startup--load-user-init-file (:override (&rest _) init-doom 100)
  (let ((debug-on-error-from-init-file nil)
        (debug-on-error-should-be-set nil)
        (debug-on-error-initial (if (eq init-file-debug t) 'startup init-file-debug))
        ;; The init file might contain byte-code with embedded NULs, which can
        ;; cause problems when read back, so disable nul byte detection. (Bug
        ;; #52554)
        (inhibit-null-byte-detection t))
    (let ((debug-on-error debug-on-error-initial))
      (condition-case-unless-debug error
          (when init-file-user
            (let ((init-file-name
                   ;; This dynamically generated init file stores a lot of
                   ;; precomputed information, such as module and package
                   ;; autoloads, and values for expensive variables like
                   ;; `doom-modules', `doom-disabled-packages', `load-path',
                   ;; `auto-mode-alist', and `Info-directory-list'. etc.
                   ;; Compiling them in one place is a big reduction in startup
                   ;; time, and by keeping a history of them, you get a snapshot
                   ;; of your config in time.
                   (doom-profile-init-file doom-profile)))
              ;; If we loaded a compiled file, set `user-init-file' to the
              ;; source version if that exists.
              (setq user-init-file
                    (concat (string-remove-suffix ".elc" init-file-name)
                            ".el"))
              ;; HACK: if `init-file-name' happens to be higher in
              ;;   `load-history' than a symbol's actual definition,
              ;;   `symbol-file' (and help/helpful buffers) will report the
              ;;   source of a symbol as `init-file-name', rather than it's true
              ;;   source. By removing this file from `load-history', no one
              ;;   will make that mistake.
              (setq load-history
                    (delete (assoc init-file-name load-history)
                            load-history))
              (doom-startup)))
        ;; TODO: Add safe-mode profile.
        ;; (error
        ;;  ;; HACK: This is not really this variable's intended purpose, but it
        ;;  ;;   doesn't mind what value its set to, only that its non-nil, so I'm
        ;;  ;;   exploiting its dynamic scope to pass the error to the profile.
        ;;  (setq init-file-had-error error)
        ;;  (load (file-name-concat doom-emacs-dir "profiles" "safe-mode" "init.el")
        ;;        nil 'nomessage 'nosuffix))
        (error
         (display-warning
          'initialization
          (format-message "\
An error occurred while booting Doom Emacs:\n\n%s%s%s\n\n\
To ensure normal operation, you should investigate and remove the
cause of the error in your Doom config files. Start Emacs with
the `--debug-init' option to view a complete error backtrace."
                          (get (car error) 'error-message)
                          (if (cdr error) ": " "")
                          (mapconcat (lambda (s) (prin1-to-string s t))
                                     (cdr error) ", "))
          :warning)
         (setq init-file-had-error t)))
      ;; If we can tell that the init file altered debug-on-error, arrange to
      ;; preserve the value that it set up.
      (or (eq debug-on-error debug-on-error-initial)
          (setq debug-on-error-should-be-set t
                debug-on-error-from-init-file debug-on-error)))
    (when debug-on-error-should-be-set
      (setq debug-on-error debug-on-error-from-init-file))))

(provide 'doom-start)
;;; doom-start.el ends here
