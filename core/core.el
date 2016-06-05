;;; core.el --- The heart of the beast
;;
;;; Naming conventions:
;;
;;   doom-...     A public variable/constant or function
;;   doom--...    An internal variable or function (non-interactive)
;;   doom/...     An autoloaded interactive function
;;   doom:...     An ex command
;;   doom|...     A hook
;;   doom*...     An advising function
;;   doom....     Custom prefix commands
;;   ...!         Macro, shortcut alias or subst defun
;;
;;; Autoloaded functions are in {core,modules}/defuns/defuns-*.el

;; Premature optimization for faster startup
(setq-default gc-cons-threshold 339430400
              gc-cons-percentage 0.6)

(defalias '! 'eval-when-compile)

(defconst doom-emacs-dir     (! (expand-file-name user-emacs-directory)))
(defconst doom-core-dir      (! (expand-file-name "core" doom-emacs-dir)))
(defconst doom-modules-dir   (! (expand-file-name "modules" doom-emacs-dir)))
(defconst doom-private-dir   (! (expand-file-name "private" doom-emacs-dir)))
(defconst doom-packages-dir  (! (expand-file-name (concat ".cask/" emacs-version "/elpa") doom-emacs-dir)))
(defconst doom-ext-dir       (! (expand-file-name "ext" doom-emacs-dir)))
(defconst doom-themes-dir    (! (expand-file-name "themes" doom-private-dir)))
(defconst doom-temp-dir
  (! (format "%s/cache/%s/%s.%s"
             doom-private-dir (system-name)
             emacs-major-version emacs-minor-version))
  "Hostname and emacs-version-based elisp temp directories")

;; window-system is deprecated. Not on my watch!
(unless (boundp 'window-system)
  (defvar window-system (framep-on-display)))

;;
;; Load path
;;

(defvar doom--load-path load-path
  "Initial `load-path'; so we don't clobber it on consecutive reloads.")

(defsubst --subdirs (path &optional include-self)
  (let ((result (if include-self (list path) (list))))
    (mapc (lambda (file)
            (when (file-directory-p file)
              (push file result)))
          (ignore-errors (directory-files path t "^[^.]" t)))
    result))

;; Populate the load-path manually; cask shouldn't be an internal dependency
(setq load-path
      (! (append (list doom-private-dir)
                 (--subdirs doom-core-dir t)
                 (--subdirs doom-modules-dir t)
                 (--subdirs doom-packages-dir)
                 (--subdirs (expand-file-name "../bootstrap" doom-packages-dir))
                 (--subdirs doom-themes-dir t)
                 doom--load-path))
      custom-theme-load-path
      (! (append (--subdirs doom-themes-dir t)
                 custom-theme-load-path)))

;;
;; Core configuration
;;

;; UTF-8 please
(set-charset-priority 'unicode)
(setq locale-coding-system    'utf-8)   ; pretty
(set-terminal-coding-system   'utf-8)   ; pretty
(set-keyboard-coding-system   'utf-8)   ; pretty
(set-selection-coding-system  'utf-8)   ; please
(prefer-coding-system         'utf-8)   ; with sugar on top
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; stop package.el from being annoying. I rely solely on Cask.
(setq package--init-file-ensured t
      package-enable-at-startup nil
      package-archives
      '(("gnu"   . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org"   . "http://orgmode.org/elpa/"))

      ad-redefinition-action            'accept      ; silence the advised function warnings
      compilation-always-kill            t           ; kill compl. process before spawning another
      compilation-ask-about-save         nil         ; save all buffers before compiling
      compilation-scroll-output          t           ; scroll with output while compiling
      confirm-nonexistent-file-or-buffer t
      delete-by-moving-to-trash          t
      echo-keystrokes                    0.02        ; show me what I type
      ediff-diff-options                 "-w"
      ediff-split-window-function       'split-window-horizontally  ; side-by-side diffs
      ediff-window-setup-function       'ediff-setup-windows-plain  ; no extra frames
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

(autoload 'use-package "use-package" "" nil 'macro)
(require 'f)
(require 'core-vars)
(require 'core-defuns)
(unless (require 'autoloads nil t)
  (doom-reload-autoloads)
  (unless (require 'autoloads nil t)
    (error "Autoloads weren't generated! Run `make autoloads`")))

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

(use-package help-fns+ ; Improved help commands
  :commands (describe-buffer describe-command describe-file
             describe-keymap describe-option describe-option-of-type))

(use-package smex
  :commands (smex smex-major-mode-commands)
  :config
  (setq smex-completion-method 'ivy
        smex-save-file (concat doom-temp-dir "/smex-items"))
  (smex-initialize))


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
(add-hook! emacs-startup
  ;; We add this to `after-init-hook' to allow errors to stop it
  (defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
    "Prevent annoying \"Active processes exist\" query when you quit Emacs."
    (cl-flet ((process-list ())) ad-do-it)))

(provide 'core)
;;; core.el ends here
