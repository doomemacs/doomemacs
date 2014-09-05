(provide 'core)

(defconst is-mac (eq system-type 'darwin))
(defconst is-linux (eq system-type 'gnu/linux))

;; Setup theme
(add-to-list 'custom-theme-load-path *themes-dir)
(load-theme *theme t)

;; Emacs under-the-hood
(global-auto-revert-mode 1)         ; revert buffers for changed files

(prefer-coding-system 'utf-8)
(setq-default load-prefer-newer t)  ; load newer .el over older .elc
(setq-default gc-cons-threshold 50000000) ; avoid garbage collection (default is 400k)
(setq redisplay-dont-pause t)
(fset 'yes-or-no-p 'y-or-n-p)       ; y/n instead of yes/no
(setq confirm-kill-emacs nil)
(setq-default enable-recursive-minibuffers nil)

;; Show keystrokes in [near] realtime
(setq echo-keystrokes 0.02)

;; Sane scroll settings
(setq scroll-margin 3
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(setq inhibit-startup-screen t     ; don't show EMACs start screen
      inhibit-splash-screen t
      inhibit-startup-buffer-menu t
      inhibit-startup-echo-area-message t
      initial-major-mode 'text-mode
      initial-scratch-message nil
      initial-scratch-buffer nil)   ; empty scratch buffer

;;; Backups
;; If I ever enable backups/autosaves, then change where they go
(setq make-backup-files         nil       ; Don't want any backup files
      auto-save-list-file-name  nil       ; Don't want any .saves files
      auto-save-default         nil       ; Don't want any auto saving
      create-lockfiles          nil)

(setq backup-directory-alist `((".*" . ,"/tmp/emacs/")))
(setq auto-save-file-name-transforms `((".*" ,"/tmp/emacs/" t)))
;; Save history across sessions
(setq savehist-additional-variables
      ;; search entries
      '(search ring regexp-search-ring)
      ;; save every 5 minutes
      savehist-autosave-interval 300
      ;; keep the home clean
      savehist-file (expand-file-name "savehist" "/tmp/emacs/"))
(savehist-mode 1)
;; Save cursor location across sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name "saveplace" "/tmp/emacs/"))

;; window layout undo/redo, keymaps in core-keymaps.el
(when (fboundp 'winner-mode) (winner-mode 1))

;;;; Advice ;;;;;;;;;;;;;;;;;;;;;;;;
;; Make next/previous-buffer skip special buffers
(defadvice next-buffer (after avoid-messages-buffer-in-next-buffer)
  "Advice around `next-buffer' to avoid going into the *Messages* buffer."
  (when (string-match "\\`\\*.+\\*\\'" (buffer-name)) (next-buffer)))
(defadvice previous-buffer (after avoid-messages-buffer-in-previous-buffer)
  "Advice around `previous-buffer' to avoid going into the *Messages* buffer."
  (when (string-match "\\`\\*.+\\*\\'" (buffer-name)) (previous-buffer)))

;;;; My personal minor mode ;;;;;;;;
(defvar my-mode-map (make-sparse-keymap))
(define-minor-mode my-mode :global t :keymap my-mode-map)

;; Automatic minor modes
(defvar auto-minor-mode-alist ()
  "Alist of filename patterns vs correpsonding minor mode functions, see `auto-mode-alist'
All elements of this alist are checked, meaning you can enable multiple minor modes for the same regexp.")
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
;; (when noninteractive
;;   (shut-up-silence-emacs))

;; Package management bootstrap
(setq package-enable-at-startup nil
      package-archive-exclude-alist
      '(("melpa" org-trello)
        ("melpa" org)
        ("marmalade" org)
        ("gnu" org))
      delete-old-versions t)

(let ((default-directory *elisp-dir))
    (normal-top-level-add-to-load-path '("."))
    (normal-top-level-add-subdirs-to-load-path))

(require 'use-package)
(require 'diminish)

(require 'core-ui)
(require 'core-editor)
(if is-mac (require 'core-osx))

(add-hook 'after-init-hook (lambda() (require 'my-keymaps)))
