(provide 'core)

(defconst is-mac   (eq system-type 'darwin))
(defconst is-linux (eq system-type 'gnu/linux))

;;;; Load theme ;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'custom-theme-load-path *themes-dir)
(load-theme *theme t)


;;;; Emacs under the hood ;;;;;;;;;;;;;;;;
(prefer-coding-system 'utf-8)
(fset 'yes-or-no-p 'y-or-n-p)            ; y/n instead of yes/no
(setq-default
      load-prefer-newer t                ; load newer .el over older .elc
      gc-cons-threshold 50000000         ; avoid garbage collection (default is 400k)
      enable-recursive-minibuffers nil
      redisplay-dont-pause t
      confirm-kill-emacs nil
      compilation-scroll-output t)

;; Sane scroll settings
(setq scroll-margin 5
      scroll-conservatively 100000
      scroll-preserve-screen-position 1
      echo-keystrokes 0.02               ; Show keystrokes
      inhibit-startup-screen t           ; don't show EMACs start screen
      inhibit-splash-screen t
      inhibit-startup-buffer-menu t
      initial-major-mode 'text-mode
      initial-scratch-message nil
      initial-scratch-buffer nil)        ; empty scratch buffer

;; Make sure undo/backup folders exist
(require 'f)
(defconst *tmp-dir-undo     (f-expand "undo" *tmp-dir))
(defconst *tmp-dir-backup   (f-expand "backup" *tmp-dir))
(defconst *tmp-dir-autosave (f-expand "autosave" *tmp-dir))
(unless (f-dir? *tmp-dir)
  (f-mkdir *tmp-dir
           *tmp-dir-undo
           *tmp-dir-backup
           *tmp-dir-autosave))

;; Disable all backups (that's what git/dropbox are for)
(setq make-backup-files         nil
      create-lockfiles          nil
      auto-save-default         nil
      auto-save-list-file-name  (concat *tmp-dir-autosave ".auto-save")
      ;; In case I want to reactivate backup files
      backup-directory-alist `((".*" . ,*tmp-dir-backup))
      auto-save-file-name-transforms `((".*" ,*tmp-dir-autosave t)))

;; Remember undo history
(setq-default undo-tree-history-directory-alist `(("." . ,*tmp-dir-undo)))
(setq-default undo-tree-auto-save-history t)

;;;; Save history across sessions
;; search entries
(defvar savehist-additional-variables '(search ring regexp-search-ring))
;; keep the home clean
(defvar savehist-file (f-expand "savehist" *tmp-dir))
(savehist-mode 1)


;;;; My personal global mode ;;;;;;;;;;;;;
(defvar my-mode-map (make-sparse-keymap))
(define-minor-mode my-mode
  "My personal global mode."
  :global t
  :keymap my-mode-map)


;;;; Behavior adjustments ;;;;;;;;;;;;;;;;
;; Make next/previous-buffer skip special buffers
(defadvice next-buffer (after void-messages-buffer-in-next-buffer activate)
  "Advice around `next-buffer' to avoid going into the *Messages* buffer."
  (when (string-match "\\`\\*.+\\*\\'" (buffer-name)) (next-buffer)))
(defadvice previous-buffer (after avoid-messages-buffer-in-previous-buffer activate)
  "Advice around `previous-buffer' to avoid going into the *Messages* buffer."
  (when (string-match "\\`\\*.+\\*\\'" (buffer-name)) (previous-buffer)))


;;;; Automatic minor modes ;;;;;;;;;;;;;;;
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

(global-auto-revert-mode 1)              ; revert buffers for changed files
(require 'shut-up)
(when noninteractive (shut-up-silence-emacs))

;; What we do every night, Pinkie...
(defun my/greeter () (message "What're we gonna do tonight, Brain?"))
(defalias 'display-startup-echo-area-message 'my/greeter)

(when (fboundp 'winner-mode) (winner-mode 1))  ; window layout undo/redo


;;;; Utility plugins ;;;;;;;;;;;;;;;;;;
;; Package management bootstrap
(require 'use-package)
(setq package-enable-at-startup nil
      delete-old-versions t)
;; Add ./elisp/* to load-path
(let ((default-directory *elisp-dir))
    (normal-top-level-add-to-load-path '("."))
    (normal-top-level-add-subdirs-to-load-path))

(use-package key-chord
  :init (key-chord-mode 1)
  :config (setq key-chord-two-keys-delay 0.5))

(use-package recentf
  :init
  (progn (setq recentf-max-menu-items 0
               recentf-max-saved-items 100
               recentf-auto-cleanup 'never
               recentf-save-file (concat *tmp-dir "recentf")
               recentf-exclude '("/tmp/" "/ssh:" "\\.?ido\\.last\\'" "\\.revive\\'", "/TAGS\\'"))
         (recentf-mode 1)))

(use-package smex
  :commands (smex smex-major-mode-commands)
  :config
  (progn (smex-initialize)
         ;; Hook up smex to auto-update, rather than update on every run
         (defun smex-update-after-load (unused)
           (when (boundp 'smex-cache) (smex-update)))
         (add-hook 'after-load-functions 'smex-update-after-load)))

(use-package popwin
  :init (popwin-mode 1)
  :config
  (progn (setq popwin:popup-window-height 0.45)
         (push '(diff-mode :position bottom :stick t) popwin:special-display-config)
         (push '("*Backtrace*") popwin:special-display-config)
         (push '("*Warnings*") popwin:special-display-config)
         (push '("*Process List*") popwin:special-display-config)))
