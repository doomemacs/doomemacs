;;; core.el --- The heart of the beast
;;
;;; Naming conventions:
;;
;;   doom-...     A public variable/constant or function
;;   doom--...    An internal variable or function (non-interactive)
;;   doom/...     An autoloaded function
;;   doom:...     An ex command
;;   doom|...     A hook
;;   doom*...     An advising function
;;   ...!         Macro, shortcut alias or subst defun
;;   @...         Autoloaded interactive lambda macro for keybinds
;;
;;; Autoloaded functions are in {core,modules}/defuns/defuns-*.el

;; Premature optimization for faster startup
(setq-default gc-cons-threshold 339430400
              gc-cons-percentage 0.6)

;;
;; Global Constants
;;

(defconst doom-version "1.3.0"
  "Current version of DOOM emacs")

(defconst doom-emacs-dir
  (expand-file-name user-emacs-directory)
  "The path to this emacs.d directory")

(defconst doom-core-dir
  (expand-file-name "core" doom-emacs-dir)
  "Where essential files are stored")

(defconst doom-modules-dir
  (expand-file-name "modules" doom-emacs-dir)
  "Where configuration modules are stored")

(defconst doom-private-dir
  (expand-file-name "private" doom-emacs-dir)
  "Where private configuration filse and assets are stored (like snippets)")

(defconst doom-packages-dir
  (expand-file-name
   (format ".cask/%s.%s/elpa" emacs-major-version emacs-minor-version)
   doom-emacs-dir)
  "Where plugins are installed (by cask)")

(defconst doom-ext-dir
  (expand-file-name "ext" doom-emacs-dir)
  "Where external dependencies are stored (like libraries or binaries)")

(defconst doom-themes-dir
  (expand-file-name "themes" doom-private-dir)
  "Where theme files and subfolders go")

(defconst doom-temp-dir
  (format "%s/cache/%s" doom-private-dir (system-name))
  "Hostname-based elisp temp directories")

;; window-system is deprecated. Not on my watch!
(unless (boundp 'window-system)
  (defvar window-system (framep-on-display)))

;;
(defconst doom-leader      ","  "Prefix for <leader> bindings")
(defconst doom-localleader "\\" "Prefix for <localleader> bindings")

(defvar doom-unreal-buffers
  '("^ ?\\*.+" image-mode dired-mode reb-mode messages-buffer-mode
    tabulated-list-mode comint-mode magit-mode)
  "A list of regexps or modes whose buffers are considered unreal, and will be
ignored when using `doom:next-real-buffer' and `doom:previous-real-buffer' (or
killed by `doom/kill-unreal-buffers', or after `doom/kill-real-buffer').")

(defvar doom-cleanup-processes-alist
  '(("pry" . ruby-mode)
    ("irb" . ruby-mode)
    ("ipython" . python-mode))
  "An alist of (process-name . major-mode) that `doom/kill-process-buffers'
checks before killing processes. If there are no buffers with matching
major-modes, the process gets killed.")

(defvar doom-unicode-font
  (font-spec :family "DejaVu Sans Mono" :size 12)
  "Fallback font for unicode glyphs.")


;;
;; Core configuration
;;

;; UTF-8 as the default coding system, please
(set-charset-priority 'unicode)        ; pretty
(prefer-coding-system        'utf-8)   ; pretty
(set-terminal-coding-system  'utf-8)   ; pretty
(set-keyboard-coding-system  'utf-8)   ; perdy
(set-selection-coding-system 'utf-8)   ; please
(setq locale-coding-system   'utf-8)   ; with sugar on top

;; default-buffer-file-coding-system is deprecated on 23.2
(if (boundp 'buffer-file-coding-system)
    (setq-default buffer-file-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8))

;; Don't pester me package.el. Cask is my one and only.
(setq-default
 package--init-file-ensured t
 package-user-dir doom-packages-dir
 package-enable-at-startup nil
 package-archives
 '(("gnu"   . "http://elpa.gnu.org/packages/")
   ("melpa" . "http://melpa.org/packages/")
   ("org"   . "http://orgmode.org/elpa/")))

;; Core settings
(setq ad-redefinition-action            'accept      ; silence the advised function warnings
      apropos-do-all                     t           ; make `apropos' more useful
      byte-compile-warnings              nil
      compilation-always-kill            t           ; kill compl. process before spawning another
      compilation-ask-about-save         nil         ; save all buffers before compiling
      compilation-scroll-output          t           ; scroll with output while compiling
      confirm-nonexistent-file-or-buffer t
      delete-by-moving-to-trash          t
      echo-keystrokes                    0.02        ; show me what I type
      enable-recursive-minibuffers       nil         ; no minibufferception
      idle-update-delay                  5           ; update a little less often
      major-mode                        'text-mode
      ring-bell-function                'ignore      ; silence of the bells!
      save-interprogram-paste-before-kill nil
      sentence-end-double-space          nil
      ;; http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
      minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)
      ;; persistent bookmarks
      bookmark-save-flag                 t
      bookmark-default-file              (concat doom-temp-dir "/bookmarks")
      ;; Disable backups (that's what git/dropbox are for)
      history-length                     1000
      vc-make-backup-files               nil
      auto-save-default                  nil
      auto-save-list-file-name           (concat doom-temp-dir "/autosave")
      make-backup-files                  nil
      create-lockfiles                   nil
      backup-directory-alist            `((".*" . ,(concat doom-temp-dir "/backup/")))
      ;; Remember undo history
      undo-tree-auto-save-history        nil
      undo-tree-history-directory-alist `(("." . ,(concat doom-temp-dir "/undo/"))))


;;
;; Bootstrap
;;

(defvar doom--load-path load-path
  "Initial `load-path', used as a base so we don't clobber it on consecutive
reloads.")

(defvar doom-packages '()
  "A list of all installed packages. Filled internally; do not edit it!")

;; Just the bear necessities... ♫
(setq load-path (append (list doom-core-dir) doom--load-path))

;; Populate load-path manually. This way, cask (and `cask-initialize') won't be
;; an internal dependency -- they slow down startup a lot!
(require 'core-defuns)
(let ((paths (eval-when-compile (doom-reload))))
  (setq load-path (car paths)
        custom-theme-load-path (nth 1 paths)
        doom-packages (nth 2 paths)))

;; Many functions are lazy-loaded. The autoloads.el file contains info on where
;; to find them if they're called. Tries to generate autoloads.el if one isn't
;; found.
(unless (require 'autoloads nil t)
  (doom-reload-autoloads)
  (unless (require 'autoloads nil t)
    (error "Autoloads weren't generated! Run `make autoloads`")))


;;
;; Automatic minor modes
;;

(defvar doom-auto-minor-mode-alist '()
  "Alist of filename patterns vs corresponding minor mode functions, see
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


;;
;; Essential plugins
;;

(require 'dash)
(require 's)
(require 'f)

(autoload 'use-package "use-package" "" nil 'macro)

(use-package anaphora
  :commands (awhen aif acond awhile))

(use-package persistent-soft
  :commands (persistent-soft-store
             persistent-soft-fetch
             persistent-soft-exists-p
             persistent-soft-flush
             persistent-soft-location-readable
             persistent-soft-location-destroy)
  :init (defvar pcache-directory (concat doom-temp-dir "/pcache/")))

(use-package async
  :commands (async-start
             async-start-process
             async-get
             async-wait
             async-inject-variables))

(use-package json
  :commands (json-read-from-string json-encode json-read-file))

(provide 'core)
;;; core.el ends here
