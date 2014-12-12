(cd "~") ; instead of /

(require 'cask)
(cask-initialize)

(defconst is-mac   (eq system-type 'darwin))
(defconst is-linux (eq system-type 'gnu/linux))

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
  (electric-indent-mode -1)    ; In case of emacs 24.4

  ;;; window layout undo/redo
  (winner-mode 1)
  (setq winner-boring-buffers '("*Completions*" "*Help*"))

  ;;; UTF-8 please
  (setq locale-coding-system 'utf-8)    ; pretty
  (set-terminal-coding-system 'utf-8)   ; pretty
  (set-keyboard-coding-system 'utf-8)   ; pretty
  (set-selection-coding-system 'utf-8)  ; please
  (prefer-coding-system 'utf-8)         ; with sugar on top

  (fset 'yes-or-no-p 'y-or-n-p)            ; y/n instead of yes/no

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
  (setq savehist-file (f-expand "savehist" my-tmp-dir)) ; keep the home clean
  (savehist-mode 1)

  ;; Save cursor location across sessions
  (require 'saveplace)
  (setq save-place-file (f-expand "saveplace" my-tmp-dir))
  (add-hook 'find-file-hook ; activate save-place for files that exist
            (lambda()
              (if (file-exists-p buffer-file-name)
                  (setq save-place t))))

  (require 'recentf)
  (setq recentf-max-menu-items 0)
  (setq recentf-max-saved-items 1000)
  (setq recentf-auto-cleanup 'never)
  (setq recentf-save-file (concat my-tmp-dir "recentf"))
  (setq recentf-exclude '("/tmp/" "/ssh:" "\\.?ido\\.last\\'" "\\.revive\\'", "/TAGS\\'"))
  (recentf-mode 1)

  ;; What we do every night, Pinkie...
  (defun display-startup-echo-area-message ()
    (message "What're we gonna do tonight, Brain? (Loaded in %s)" (emacs-init-time)))


  ;;;; Editor behavior ;;;;;;;;;;;;;;;;
  ;; spaces instead of tabs
  (setq-default indent-tabs-mode nil)     ; spaces instead of tabs
  (setq-default tab-always-indent nil)
  (setq-default tab-width 4)

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


  ;;;; Utility plugins ;;;;;;;;;;;;;;;;;;
  (require 'defuns)       ; all the defuns
  (require 'use-package)  ; Package management bootstrap
  (setq use-package-verbose DEBUG-MODE)
  ;;(require 'hide-mode-line)

  ;; Generate autoloads if necessary
  (defun my-update-autoloads (&optional forcep)
    (interactive)
    (setq generated-autoload-file (concat my-core-dir "autoloads.el"))
    (when (or forcep (not (file-exists-p generated-autoload-file)))
      (if forcep (delete-file generated-autoload-file))
      (update-directory-autoloads my-core-dir my-modules-dir my-elisp-dir))
    (require 'autoloads))

  (my-update-autoloads)

  ;; Add elisp dirs to load-path
  (let ((default-directory my-elisp-dir))
    (normal-top-level-add-to-load-path '("."))
    (normal-top-level-add-subdirs-to-load-path))

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
    :init (popwin-mode 1)
    :config
    (progn ; popwin config
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
  (require 'core-ui)
  (require 'core-evil)
  (require 'core-editor)

  (require 'server)
  (unless (server-running-p)
    (server-start)))


(provide 'core)
;;; core.el ends here
