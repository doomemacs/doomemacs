;; Setup theme
(add-to-list 'custom-theme-load-path my/themes-dir)
(load-theme my/theme t)

;; Emacs under-the-hood
(prefer-coding-system 'utf-8)
(global-auto-revert-mode 1)         ; revert buffers for changed files
(setq-default load-prefer-newer t)  ; load newer .el over older .elc
(setq redisplay-dont-pause t)
(setq-default gc-cons-threshold 50000000) ; avoid garbage collection (default is 400k)
(setq make-backup-files         nil       ; Don't want any backup files
      auto-save-list-file-name  nil       ; Don't want any .saves files
      auto-save-default         nil)      ; Don't want any auto saving
(fset 'yes-or-no-p 'y-or-n-p)       ; y/n instead of yes/no
(setq confirm-kill-emacs nil)

;; Sane scroll settings
(setq scroll-margin 3
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(setq inhibit-startup-screen t     ; don't show EMACs start screen
      inhibit-splash-screen t
      inhibit-startup-buffer-menu t
      inhibit-startup-echo-area-message t
      initial-major-mode 'text-mode
      initial-scratch-message nil)

(setq-default use-dialog-box nil)
(setq-default enable-recursive-minibuffers nil)
(setq-default visible-bell nil)
(setq sentence-end-double-space nil)

;; If I ever enable backups/autosaves, then change where they go
(setq backup-directory-alist `((".*" . ,my/tmp-dir)))
(setq auto-save-file-name-transforms `((".*" ,my/tmp-dir t)))

;; window layout undo/redo, keymaps in core-keymaps.el
(when (fboundp 'winner-mode) (winner-mode 1))

(defconst is-mac
  (eq system-type 'darwin)
  "Is this running on OS X?")
(defconst is-linux
  (eq system-type 'gnu/linux)
  "Is this running on Linux?")

;;;; Advice ;;;;;;;;;;;;;;;;;;;;;;;;
;; Make next/previous-buffer skip special buffers
(defadvice next-buffer (after avoid-messages-buffer-in-next-buffer)
  "Advice around `next-buffer' to avoid going into the *Messages* buffer."
  (when (string= "*Messages*" (buffer-name))
    (next-buffer)))
(defadvice previous-buffer (after avoid-messages-buffer-in-previous-buffer)
  "Advice around `previous-buffer' to avoid going into the *Messages* buffer."
  (when (string= "*Messages*" (buffer-name))
    (previous-buffer)))

;;;; My personal minor mode ;;;;;;;;
(defvar my/mode-map (make-sparse-keymap))
(define-minor-mode my/mode :keymap my/mode-map :global t)

;;;; Load the rest ;;;;;;;;;;;;;;;;;;
(require 'core-packages)
(require 'core-ui)
(require 'core-editor)
(when is-mac (require 'core-osx))

(add-hook 'after-init-hook (lambda() (require 'core-keymaps)))

;;
(provide 'core)
