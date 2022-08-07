;;; lisp/doom-start.el --- bootstrapper for interactive sessions -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'doom-modules)


;;
;;; Reasonable defaults for interactive sessions

;; GUIs are inconsistent across systems, will rarely match our active Emacs
;; theme, and impose their shortcut key paradigms suddenly. Let's just avoid
;; them altogether and have Emacs handle the prompting.
(setq use-dialog-box nil)
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))
(when IS-LINUX
  (setq x-gtk-use-system-tooltips nil))

;; Favor vertical splits over horizontal ones, since monitors are trending
;; toward wide rather than tall.
(setq split-width-threshold 160
      split-height-threshold nil)


;;
;;; MODE-local-vars-hook

;; File+dir local variables are initialized after the major mode and its hooks
;; have run. If you want hook functions to be aware of these customizations, add
;; them to MODE-local-vars-hook instead.
(defvar doom-inhibit-local-var-hooks nil)

(defun doom-run-local-var-hooks-h ()
  "Run MODE-local-vars-hook after local variables are initialized."
  (unless (or doom-inhibit-local-var-hooks delay-mode-hooks)
    (setq-local doom-inhibit-local-var-hooks t)
    (doom-run-hooks (intern (format "%s-local-vars-hook" major-mode)))))

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
      (let* ((gc-cons-threshold most-positive-fixnum)
             (req (pop packages)))
        (unless (featurep req)
          (doom-log "Incrementally loading %s" req)
          (condition-case-unless-debug e
              (or (while-no-input
                    ;; If `default-directory' is a directory that doesn't exist
                    ;; or is unreadable, Emacs throws up file-missing errors, so
                    ;; we set it to a directory we know exists and is readable.
                    (let ((default-directory doom-emacs-dir)
                          (inhibit-message t)
                          file-name-handler-alist)
                      (require req nil t))
                    t)
                  (push req packages))
            (error
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
;;; Let 'er rip

(defvar doom-init-time nil
  "The time it took, in seconds, for Doom Emacs to initialize.")

(defun doom-display-benchmark-h (&optional return-p)
  "Display a benchmark including number of packages and modules loaded.

If RETURN-P, return the message as a string instead of displaying it."
  (funcall (if return-p #'format #'message)
           "Doom loaded %d packages across %d modules in %.03fs"
           (- (length load-path) (length (get 'load-path 'initial-value)))
           (hash-table-count doom-modules)
           (or doom-init-time
               (setq doom-init-time
                     (float-time (time-subtract (current-time) before-init-time))))))

;; Add support for additional file extensions.
(dolist (entry '(("/\\.doomrc\\'" . emacs-lisp-mode)
                 ("/LICENSE\\'" . text-mode)
                 ("\\.log\\'" . text-mode)
                 ("rc\\'" . conf-mode)
                 ("\\.\\(?:hex\\|nes\\)\\'" . hexl-mode)))
  (push entry auto-mode-alist))

;; Doom caches a lot of information in `doom-autoloads-file'. Module and package
;; autoloads, autodefs like `set-company-backend!', and variables like
;; `doom-modules', `doom-disabled-packages', `load-path', `auto-mode-alist', and
;; `Info-directory-list'. etc. Compiling them into one place is a big reduction
;; in startup time.
(condition-case-unless-debug e
    ;; Avoid `file-name-sans-extension' for premature optimization reasons.
    ;; `string-remove-suffix' is cheaper because it performs no file sanity
    ;; checks; just plain ol' string manipulation.
    (load (string-remove-suffix ".el" doom-autoloads-file) nil 'nomessage)
  (file-missing
   ;; If the autoloads file fails to load then the user forgot to sync, or
   ;; aborted a doom command midway!
   (if (locate-file doom-autoloads-file load-path)
       ;; Something inside the autoloads file is triggering this error;
       ;; forward it to the caller!
       (signal 'doom-autoload-error e)
     (signal 'doom-error
             (list "Doom is in an incomplete state"
                   "run 'doom sync' on the command line to repair it")))))

(when (and (or (display-graphic-p)
               (daemonp))
           doom-env-file)
  (setq-default process-environment (get 'process-environment 'initial-value))
  (doom-load-envvars-file doom-env-file 'noerror))

;; Bootstrap the interactive session
(add-hook 'after-change-major-mode-hook #'doom-run-local-var-hooks-h 100)
(add-hook 'hack-local-variables-hook #'doom-run-local-var-hooks-h)
(add-hook 'emacs-startup-hook #'doom-load-packages-incrementally-h)
(add-hook 'window-setup-hook #'doom-display-benchmark-h 105)
(doom-run-hook-on 'doom-first-buffer-hook '(find-file-hook doom-switch-buffer-hook))
(doom-run-hook-on 'doom-first-file-hook   '(find-file-hook dired-initial-position-hook))
(doom-run-hook-on 'doom-first-input-hook  '(pre-command-hook))

(add-hook 'doom-first-buffer-hook #'gcmh-mode)

;; There's a chance the user will later use package.el or straight in this
;; interactive session. If they do, make sure they're properly initialized
;; when they do.
(autoload 'doom-initialize-packages "doom-packages")
(eval-after-load 'package '(require 'doom-packages))
(eval-after-load 'straight '(doom-initialize-packages))

;; Load all things.
(doom-initialize-modules)

(provide 'doom-start)
;;; doom-start.el ends here
