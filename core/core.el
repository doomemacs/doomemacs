;;; core.el --- the heart of the beast -*- lexical-binding: t; -*-

(eval-when-compile
  (when (version< emacs-version "25")
    (error "Doom only supports Emacs 25.1 and higher!")))

;;
(defvar doom-version "2.0.9"
  "Current version of DOOM emacs.")

(defvar doom-debug-mode (or (getenv "DEBUG") init-file-debug)
  "If non-nil, all doom functions will be verbose. Set DEBUG=1 in the command
line or use --debug-init to enable this.")

(defconst EMACS26+
  (eval-when-compile (not (version< emacs-version "26"))))
(defconst EMACS27+
  (eval-when-compile (not (version< emacs-version "27"))))

;;
(defvar doom-emacs-dir
  (eval-when-compile (file-truename user-emacs-directory))
  "The path to this emacs.d directory. Must end in a slash.")

(defvar doom-core-dir (concat doom-emacs-dir "core/")
  "Where essential files are stored.")

(defvar doom-modules-dir (concat doom-emacs-dir "modules/")
  "The main directory where Doom modules are stored.")

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

(defvar doom-private-dir
  (eval-when-compile
    (or (let ((xdg-path
               (expand-file-name "doom/"
                                 (or (getenv "XDG_CONFIG_HOME")
                                     "~/.config"))))
          (if (file-directory-p xdg-path) xdg-path))
        "~/.doom.d/"))
  "Where your private customizations are placed. Must end in a slash. Respects
XDG directory conventions if ~/.config/doom exists.")

;; Doom hooks
(defvar doom-init-hook nil
  "Hooks run after all init.el files are loaded, including your private and all
module init.el files, but before their config.el files are loaded.")

(defvar doom-post-init-hook nil
  "A list of hooks run when Doom is fully initialized. Fires at the end of
`emacs-startup-hook', as late as possible. Guaranteed to run after everything
else (except for `window-setup-hook').")


;;;
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
 debug-on-error doom-debug-mode
 ffap-machine-p-known 'reject     ; don't ping things that look like domain names
 idle-update-delay 2              ; update ui less often
 auto-mode-case-fold nil
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
 ;; files
 abbrev-file-name             (concat doom-local-dir "abbrev.el")
 auto-save-list-file-name     (concat doom-cache-dir "autosave")
 backup-directory-alist       (list (cons "." (concat doom-cache-dir "backup/")))
 custom-file                  (concat doom-etc-dir "custom.el")
 mc/list-file                 (concat doom-etc-dir "mc-lists.el")
 pcache-directory             (concat doom-cache-dir "pcache/")
 request-storage-directory    (concat doom-cache-dir "request")
 server-auth-dir              (concat doom-cache-dir "server/")
 shared-game-score-directory  (concat doom-etc-dir "shared-game-score/")
 tramp-auto-save-directory    (concat doom-cache-dir "tramp-auto-save/")
 tramp-backup-directory-alist backup-directory-alist
 tramp-persistency-file-name  (concat doom-cache-dir "tramp-persistency.el")
 url-cache-directory          (concat doom-cache-dir "url/")
 url-configuration-directory  (concat doom-etc-dir "url/"))
(load custom-file t t t)


;;
;; Emacs fixes/hacks
;;

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
        (if (string-match-p (caar alist) name)
            (funcall (cdar alist) 1))
        (setq alist (cdr alist))))))
(add-hook 'find-file-hook #'doom|enable-minor-mode-maybe)

(defun doom*set-indirect-buffer-filename (orig-fn base-buffer name &optional clone)
  "In indirect buffers, `buffer-file-name' is nil, which can cause problems
with functions that require it (like modeline segments)."
  (let ((file-name (buffer-file-name base-buffer))
        (buffer (funcall orig-fn base-buffer name clone)))
    (when (and file-name buffer)
      (with-current-buffer buffer
        (unless buffer-file-name
          (setq buffer-file-name file-name
                buffer-file-truename (file-truename file-name)))))
    buffer))
(advice-add #'make-indirect-buffer :around #'doom*set-indirect-buffer-filename)

;; Truly silence startup message
(fset #'display-startup-echo-area-message #'ignore)


;;
;; Optimize startup
;;

(defvar doom--file-name-handler-alist file-name-handler-alist)
(unless (or after-init-time noninteractive)
  ;; A big contributor to long startup times is the garbage collector, so we up
  ;; its memory threshold, temporarily and reset it later in `doom|finalize'.
  (setq gc-cons-threshold 402653184
        gc-cons-percentage 1.0
        ;; consulted on every `require', `load' and various file reading
        ;; functions. You get a minor speed up by nooping this.
        file-name-handler-alist nil))

(defun doom|finalize ()
  "Resets garbage collection settings to reasonable defaults (if you don't do
this, you'll get stuttering and random freezes) and resets
`file-name-handler-alist'."
  (setq file-name-handler-alist doom--file-name-handler-alist
        gc-cons-threshold 16777216
        gc-cons-percentage 0.2))

(add-hook 'emacs-startup-hook #'doom|finalize)
(add-hook 'doom-reload-hook   #'doom|finalize)


;;
;; Bootstrap Doom
;;

(add-to-list 'load-path doom-core-dir)

(require 'core-lib)
(require 'core-packages)

(doom-initialize noninteractive)
(if noninteractive
    (require 'core-dispatcher)
  (doom-initialize-modules))

(provide 'core)
;;; core.el ends here
