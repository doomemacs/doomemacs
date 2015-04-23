(defconst is-mac   (eq system-type 'darwin))
(defconst is-linux (eq system-type 'gnu/linux))

(when is-linux (add-to-list 'load-path "~/.cask"))
(setq use-package-verbose DEBUG-MODE)

(cd "~") ; instead of /

;; Make sure undo/backup folders exist
(defconst my-tmp-dir-undo     (expand-file-name "undo" my-tmp-dir))
(defconst my-tmp-dir-backup   (expand-file-name "backup" my-tmp-dir))
(defconst my-tmp-dir-autosave (expand-file-name "autosave" my-tmp-dir))
(unless (file-directory-p my-tmp-dir)
   (make-directory my-tmp-dir-undo t)
   (make-directory my-tmp-dir-backup t)
   (make-directory my-tmp-dir-autosave t))

(setq load-prefer-newer t)
(setq debug-on-quit DEBUG-MODE)

(require 'shut-up)
(setq shut-up-ignore DEBUG-MODE)
(when noninteractive (shut-up-silence-emacs)) ; http://youtu.be/Z6woIRLnbmE

(shut-up
  ;;;; Sane defaults ;;;;;;;;;;;;;;;;;;;;;;;
  (line-number-mode 1)         ; hide line no in modeline
  (column-number-mode 1)       ; hide col no in modeline
  (auto-compression-mode t)    ; Transparently open compressed files
  (global-font-lock-mode t)    ; Enable syntax highlighting for older emacs
  (global-auto-revert-mode 1)  ; revert buffers for changed files
  (electric-indent-mode -1)    ; In case of emacs >24.4

  ;;; window layout undo/redo
  (winner-mode 1)
  (setq winner-boring-buffers '("*Completions*" "*Help*"))

  ;;; UTF-8 please
  (setq locale-coding-system 'utf-8)    ; pretty
  (set-terminal-coding-system 'utf-8)   ; pretty
  (set-keyboard-coding-system 'utf-8)   ; pretty
  (set-selection-coding-system 'utf-8)  ; please
  (prefer-coding-system 'utf-8)         ; with sugar on top

  (fset 'yes-or-no-p 'y-or-n-p)         ; y/n instead of yes/no

  ;;; Show tab characters
  ;; (global-whitespace-mode 1)
  (setq whitespace-style '(trailing face tabs tab-mark) ; needs to be re-set in every buffer
        whitespace-display-mappings
        '((tab-mark   ?\t   [?| ?\t] [?\\ ?\t])
          (newline-mark 10 [36 10])))         ; for whitespace-newline-mode

  ;; avoid garbage collection (default is 400k)
  (setq-default gc-cons-threshold 20000000)
  (setq-default confirm-kill-emacs nil)

  (setq-default fill-column 80)

  ;; minibufferception? Nay!
  (setq-default enable-recursive-minibuffers nil)

  ;; Sane scroll settings
  (setq scroll-margin 5)
  (setq scroll-conservatively 9999)
  (setq scroll-preserve-screen-position 1)

  ;; Show me those keystrokes
  (setq echo-keystrokes 0.02)

  ;; I'll use visual mode, kthxbai
  (setq shift-select-mode nil)

  (setq ring-bell-function 'ignore)

  (setq inhibit-startup-screen t)           ; don't show EMACs start screen
  (setq inhibit-splash-screen t)
  (setq inhibit-startup-buffer-menu t)

  (setq initial-major-mode 'text-mode)      ; initial scratch buffer mode
  (setq initial-scratch-message nil)
  (setq initial-scratch-buffer nil)         ; empty scratch buffer

  (setq compilation-always-kill t)
  (setq compilation-ask-about-save nil)
  (setq compilation-scroll-output t)

  (setq sentence-end-double-space nil)      ; sentences end with periods. Period.

  (setq eval-expression-print-level nil)

  (setq show-paren-delay 0)

  (setq ediff-diff-options "-w")
  (setq ediff-split-window-function 'split-window-horizontally)  ; side-by-side diffs
  (setq ediff-window-setup-function 'ediff-setup-windows-plain) ; no extra frames

  ;; Fixes C-i's synonymity with TAB
  (keyboard-translate ?\C-i ?\H-i)

  ;;;; Backup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Disable all backups (that's what git/dropbox are for)
  (setq bookmark-save-flag        t)
  (setq bookmark-default-file     (expand-file-name "bookmarks" my-tmp-dir))
  (setq auto-save-default         nil)
  (setq auto-save-list-file-name  (expand-file-name ".auto-save" my-tmp-dir-autosave))
  (setq auto-save-file-name-transforms  `((".*" ,my-tmp-dir-autosave t)))
  ;; In case I want to reactivate backup files
  (setq make-backup-files         nil)
  (setq create-lockfiles          nil)
  (setq backup-directory-alist `((".*" . ,my-tmp-dir-backup)))
  ;; Remember undo history
  (setq-default undo-tree-auto-save-history t)
  (setq-default undo-tree-history-directory-alist `(("." . ,my-tmp-dir-undo)))
  ;;;; Save history across sessions
  (setq savehist-additional-variables '(search ring regexp-search-ring))
  (setq savehist-file (concat my-tmp-dir "savehist")) ; keep the home clean
  (savehist-mode 1)

  ;; Save cursor location across sessions
  (use-package saveplace
    :init
    (add-hook 'find-file-hook ; activate save-place for files that exist
      (lambda()
        (if (file-exists-p buffer-file-name)
            (setq save-place t))))
    :config
    (setq save-place-file (concat my-tmp-dir "saveplace")))

  (use-package recentf
    :config
    (progn
      (setq recentf-max-menu-items 0)
      (setq recentf-max-saved-items 1000)
      (setq recentf-auto-cleanup 'never)
      (setq recentf-save-file (concat my-tmp-dir "recentf"))
      (setq recentf-exclude '("/tmp/" "/ssh:" "\\.?ido\\.last\\'" "\\.revive\\'", "/TAGS\\'"))
      (recentf-mode 1)))

  ;; What we do every night, Pinkie...
  (defun display-startup-echo-area-message ()
    (message "What're we gonna do tonight, Brain? (Loaded in %s)" (emacs-init-time)))


  ;;;; Editor behavior ;;;;;;;;;;;;;;;;
  ;; spaces instead of tabs
  (setq-default indent-tabs-mode nil)     ; spaces instead of tabs
  (setq-default tab-always-indent t)
  (setq-default tab-width 4)

  (setq require-final-newline t)

  (setq delete-trailing-lines nil)
  (add-hook 'makefile-mode-hook 'indent-tabs-mode) ; Use normal tabs in makefiles
  ;; Make sure scratch buffer is always "in a project"
  (add-hook 'find-file-hook
    (lambda()
      (let ((buffer (get-buffer "*scratch*"))
            (pwd (my--project-root)))
        (when (buffer-live-p buffer)
          (save-window-excursion
            (switch-to-buffer buffer)
            (unless (eq (my--project-root) pwd)
              (cd pwd)
              (rename-buffer (format "*scratch* (%s)" (file-name-base (directory-file-name pwd))))))))))

  ;; My own minor mode!
  (define-minor-mode my-mode :global t :keymap (make-sparse-keymap))


  ;;;; Behavior adjustments ;;;;;;;;;;;;;;;;
  ;; Skip special buffers on next/previous-buffer or kill-this-buffer
  (defadvice next-buffer (after void-messages-buffer-in-next-buffer activate)
    (let ((buffer-name (buffer-name)))
      (when (and (string-match-p "\\`\\(\\*.+\\*\\|TAGS\\)$" buffer-name)
                 (not (string-match-p "\\`\\*scratch*" buffer-name)))
        (next-buffer))))
  (defadvice previous-buffer (after avoid-messages-buffer-in-previous-buffer activate)
    (let ((buffer-name (buffer-name)))
      (when (and (string-match-p "\\`\\(\\*.+\\*\\|TAGS\\)$" buffer-name)
                 (not (string-match-p "\\`\\*scratch*" buffer-name)))
        (previous-buffer))))
  (defadvice kill-this-buffer (after kill-this-buffer-no-switch-to-special-buffers activate)
    (let ((buffer-name (buffer-name)))
      (if (and (string-match-p "^\\*.+\\*" buffer-name)
               (not (string-match-p "^\\*scratch\\*" buffer-name)))
          (previous-buffer))))
  ;; Don't kill the scratch buffer, just empty and bury it
  (defadvice kill-this-buffer (around kill-this-buffer-or-empty-scratch activate)
    (if (string-match-p "^\\*scratch\\*" (buffer-name))
        (bury-buffer)
      ad-do-it))


  ;;;; Utility plugins ;;;;;;;;;;;;;;;;;;
  (require 'defuns)
  (require 'autoloads) ; use make autoloads to generate autoloads file

  (use-package smex
    :commands (smex smex-major-mode-commands)
    :config
    (progn
      (smex-initialize)
      ;; Hook up smex to auto-update, rather than update on every run
      (defun smex-update-after-load (unused)
        (when (boundp 'smex-cache) (smex-update)))
      (add-hook 'after-load-functions 'smex-update-after-load)))

  (use-package popwin
    :config
    (progn ; popwin config
      (popwin-mode 1)
      (setq popwin:popup-window-height 0.45)

      (push '(diff-mode :position bottom :stick t) popwin:special-display-config)
      (push '("*Backtrace*") popwin:special-display-config)
      (push '("*Warnings*") popwin:special-display-config)
      (push '("*Process List*") popwin:special-display-config)
      (push '("*Compile-Log*" :height 0.3 :position bottom :noselect t) popwin:special-display-config)
      (push '(" *undo-tree*" :width 0.3 :position right) popwin:special-display-config)
      (push '("^\\*scratch\\*.*" :regexp t :stick t) popwin:special-display-config)
      (push '(image-mode) popwin:special-display-config)

      (after "evil"
        (evil-ex-define-cmd "l[ast]" 'popwin:popup-last-buffer)
        (evil-ex-define-cmd "m[sg]" 'popwin:messages))

      (defun popwin:toggle-popup-window ()
        (interactive)
        (if (popwin:popup-window-live-p)
            (popwin:close-popup-window)
          (popwin:popup-last-buffer)))))


  ;;;; Start the party ;;;;;;;;;;;;;;;;;;;
  (if is-mac (require 'core-osx))
  ;; (if is-linux (require 'core-linux))

  (require 'core-ui)
  (require 'core-evil)
  (require 'core-editor)

  (use-package server
    :config
    (unless (server-running-p)
      (server-start))))


(provide 'core)
;;; core.el ends here
