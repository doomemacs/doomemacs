(provide 'core)

(require 'f)

(defconst is-mac (eq system-type 'darwin))
(defconst is-linux (eq system-type 'gnu/linux))

;; Setup theme
(add-to-list 'custom-theme-load-path *themes-dir)
(load-theme *theme t)

;; Emacs under-the-hood
(global-auto-revert-mode 1)         ; revert buffers for changed files
(fset 'yes-or-no-p 'y-or-n-p)       ; y/n instead of yes/no

(prefer-coding-system 'utf-8)
(setq-default load-prefer-newer t           ; load newer .el over older .elc
              gc-cons-threshold 50000000    ; avoid garbage collection (default is 400k)
              enable-recursive-minibuffers nil
              redisplay-dont-pause t
              confirm-kill-emacs nil
              vc-follow-symlinks nil
              compilation-scroll-output t)

;; Show keystrokes
(setq echo-keystrokes 0.02)

;; Sane scroll settings
(setq scroll-margin 3
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(setq inhibit-startup-screen t      ; don't show EMACs start screen
      inhibit-splash-screen t
      inhibit-startup-buffer-menu t
      inhibit-startup-echo-area-message t
      initial-major-mode 'text-mode
      initial-scratch-message nil
      initial-scratch-buffer nil)   ; empty scratch buffer

;;; Backups
(defconst *tmp-dir-undo   (f-expand "undo" *tmp-dir))
(defconst *tmp-dir-backup (f-expand "backup" *tmp-dir))
(unless (f-dir? *tmp-dir)
  (f-mkdir *tmp-dir *tmp-dir-undo *tmp-dir-backup))

(setq make-backup-files         nil       ; Don't want any backup
      auto-save-list-file-name  nil       ; Don't want any .saves
      auto-save-default         nil       ; Don't want any auto saving
      create-lockfiles          nil)

;; In case I want to reactivate backup files
(setq backup-directory-alist `((".*" . ,*tmp-dir-backup)))

;; window layout undo/redo, keymaps in core-keymaps.el
(when (fboundp 'winner-mode) (winner-mode 1))

;;;; Advice ;;;;;;;;;;;;;;;;;;;;;;;;
;; Make next/previous-buffer skip special buffers
(defadvice next-buffer (after avoid-messages-buffer-in-next-buffer activate)
  "Advice around `next-buffer' to avoid going into the *Messages* buffer."
  (when (string-match "\\`\\*.+\\*\\'" (buffer-name)) (next-buffer)))
(defadvice previous-buffer (after avoid-messages-buffer-in-previous-buffer activate)
  "Advice around `previous-buffer' to avoid going into the *Messages* buffer."
  (when (string-match "\\`\\*.+\\*\\'" (buffer-name)) (previous-buffer)))

;;;; My personal minor mode ;;;;;;;;
(defvar my-mode-map (make-sparse-keymap))
(define-minor-mode my-mode :global t :keymap my-mode-map)

;; Automatic minor modes
(defvar auto-minor-mode-alist ()
  "Alist of filename patterns vs correpsonding minor mode functions,
see `auto-mode-alist' All elements of this alist are checked, meaning
you can enable multiple minor modes for the same regexp.")

(defun enable-minor-mode-based-on-extension ()
  "check file name against auto-minor-mode-alist to enable minor modes
the checking happens for all pairs in auto-minor-mode-alist"
  (when buffer-file-name
    (let ((name buffer-file-name)
          (remote-id (file-remote-p buffer-file-name))
          (alist auto-minor-mode-alist))
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

(add-hook 'find-file-hook 'enable-minor-mode-based-on-extension)

;;;; Load the rest ;;;;;;;;;;;;;;;;;;
(require 'shut-up)
(when noninteractive
  (shut-up-silence-emacs))

;; Package management bootstrap
(setq package-enable-at-startup nil
      delete-old-versions t)

(let ((default-directory *elisp-dir))
    (normal-top-level-add-to-load-path '("."))
    (normal-top-level-add-subdirs-to-load-path))

(require 'use-package)
