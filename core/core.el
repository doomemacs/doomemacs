;;; core.el --- The heart of the beast

;;; Naming conventions:
;;
;;   doom-...   A public variable or function (non-interactive use)
;;   doom--...  A private variable, function (non-interactive use) or macro
;;   doom/...   An interactive function
;;   doom:...   An evil operator, motion or command
;;   doom|...   A hook
;;   doom*...   An advising function
;;   ...!       Macro, shortcut alias or defsubst
;;   @...       lambda macro for keybinds
;;   +...       Any of the above, but part of a module, e.g. +emacs-lisp|init-hook
;;
;;; Autoloaded functions are in {core,modules}/defuns/defuns-*.el

(when (version< emacs-version "25.1")
  (error "DOOM Emacs no longer supports Emacs <25.1! Time to upgrade!"))

;;;
(defvar doom-version "2.0.0"
  "Current version of DOOM emacs")

(defvar doom-debug-mode (or (getenv "DEBUG") debug-on-error)
  "If non-nil, all doom functions will be verbose. Set DEBUG=1 in the command
line or use --debug-init to enable this.")

(defvar doom-emacs-dir user-emacs-directory
  "The path to this emacs.d directory")

(defvar doom-core-dir (concat doom-emacs-dir "core/")
  "Where essential files are stored")

(defvar doom-modules-dir (concat doom-emacs-dir "modules/")
  "Where configuration modules are stored")

(defvar doom-scripts-dir (concat doom-emacs-dir "scripts/")
  "Where external dependencies are stored (like libraries or binaries)")

(defvar doom-local-dir (concat doom-emacs-dir ".local/")
  "Untracked directory for local Emacs files, including the cache
(`doom-cache-dir'), packages (`doom-packages-dir') and autoloads file.")

(defvar doom-cache-dir
  (concat doom-local-dir "cache/" (system-name) "/")
  "Hostname-based directory for temporary files.")

(defvar doom-packages-dir
  (concat doom-local-dir "packages/")
  "Where package.el and quelpa plugins (and their caches) are kept.")

(defconst IS-MAC   (eq system-type 'darwin))
(defconst IS-LINUX (eq system-type 'gnu/linux))


;;;
;; UTF-8 as the default coding system
(set-charset-priority 'unicode)        ; pretty
(prefer-coding-system        'utf-8)   ; pretty
(set-terminal-coding-system  'utf-8)   ; pretty
(set-keyboard-coding-system  'utf-8)   ; pretty
(set-selection-coding-system 'utf-8)   ; perdy
(setq locale-coding-system   'utf-8)   ; please
(setq-default buffer-file-coding-system 'utf-8) ; with sugar on top

;; Configuration
(setq ad-redefinition-accept 'accept   ; silence advised function warnings
      apropos-do-all t                 ; make `apropos' more useful
      compilation-always-kill t        ; kill compilation process before starting another
      compilation-ask-about-save nil   ; save all buffers on `compile'
      compilation-scroll-output t
      confirm-nonexistent-file-or-buffer t
      enable-recursive-minibuffers nil
      idle-update-delay 1              ; update ui less often
      ;; keep the point out of the minibuffer
      minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)
      ;; History & backup settings
      auto-save-default nil
      auto-save-list-file-name (concat doom-cache-dir "/autosave")
      backup-directory-alist (list (cons ".*" (concat doom-cache-dir "/backup/")))
      create-lockfiles nil
      history-length 1000
      make-backup-files nil
      vc-make-backup-files nil)


;;;
;; Automatic minor modes
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
                 (string-match-p (regexp-quote remote-id) name))
        (setq name (substring name (match-end 0))))
      (while (and alist (caar alist) (cdar alist))
        (if (string-match (caar alist) name)
            (funcall (cdar alist) 1))
        (setq alist (cdr alist))))))

(add-hook 'find-file-hook 'doom|enable-minor-mode-maybe)


;;;
;; Bootstrap
(setq gc-cons-threshold 339430400
      gc-cons-percentage 0.6)

(let (file-name-handler-list)
  (eval-and-compile
    (require 'core-packages (concat doom-core-dir "core-packages")))
  (eval-when-compile
    ;; Ensure cache folder exist
    (unless (file-exists-p doom-cache-dir)
      (make-directory doom-cache-dir t))
    (doom-initialize))
  (setq load-path (eval-when-compile load-path))

  (eval-and-compile
    (require 'dash)
    (require 'f)
    (require 's))

  ;;; Let 'er rip
  (require 'core-lib)
  (unless (require 'autoloads (concat doom-local-dir "autoloads.el") t)
    (doom/refresh-autoloads))

  (unless noninteractive
    (package! anaphora
      :commands (awhen aif acond awhile))

    (package! async
      :commands (async-start
                 async-start-process
                 async-byte-recompile-directory))

    (package! persistent-soft
      :preface (defvar pcache-directory (concat doom-cache-dir "pcache/"))
      :commands (persistent-soft-exists-p
                 persistent-soft-fetch
                 persistent-soft-flush
                 persistent-soft-store))

    (package! smex :commands smex)

    ;;
    (require! core-set)         ; a centralized config system; provides `set!'
    (require! core-states)      ; TODO
    (require! core-ui)          ; draw me like one of your French editors
    (require! core-popups)      ; taming sudden yet inevitable windows
    (require! core-editor)      ; baseline configuration for text editing
    (require! core-projects)))  ; making Emacs project-aware

(provide 'core)
;;; core.el ends here
