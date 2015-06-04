;;; The core of Narfy Emacs
;;
;;; Naming Conventions
;;
;; narf/*      public defun/variable
;; narf--*     private defun/variable
;; narf|*      hook defuns
;; narf:*      interactive/keybind defuns
;; :*          ex commands
;;

(setq package-enable-at-startup nil
      debug-on-quit DEBUG-MODE)
(cd "~") ; instead of /
(require 'core-splash)

;; This is kept separate so it can jumpstart emacs; this prevents the unstyled
;; flash of emacs pre-makeover.
(load-theme (if window-system DEFAULT-THEME TERM-THEME) t)
(when window-system
  (set-frame-font DEFAULT-FONT)
  (scroll-bar-mode -1)        ; no scrollbar
  (tool-bar-mode -1)          ; no toolbar
  (menu-bar-mode -1))         ; no menubar

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message ">> %s" "Are you pondering what I'm pondering, Pinky?")
(message "-----------------------------------------------------------------------------------")

(defun display-startup-echo-area-message ()
  (message "-----------------------------------------------------------------------------------")
  (message ">> %s\n>> Loaded in %s" (narf/random-comeback) (emacs-init-time)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (eq system-type 'windows-nt))

(require 'defuns)

;; NARF!
(defvar narf-mode-map (make-sparse-keymap))
(define-minor-mode narf-mode
  "Narf, yoink, poit."
  :global t :init-value t :lighter "NARF" :keymap narf-mode-map)

(defvar narf/leader-key      ",")
(defvar narf/localleader-key "\\")

;; Make sure undo/backup folders exist
(defconst TMP-DIR-UNDO     (expand-file-name "undo" TMP-DIR))
(defconst TMP-DIR-BACKUP   (expand-file-name "backup" TMP-DIR))
(defconst TMP-DIR-AUTOSAVE (expand-file-name "autosave" TMP-DIR))
(unless (file-directory-p TMP-DIR)
   (make-directory TMP-DIR-UNDO t)
   (make-directory TMP-DIR-BACKUP t)
   (make-directory TMP-DIR-AUTOSAVE t))

(fset 'yes-or-no-p 'y-or-n-p)         ; y/n instead of yes/no

;;;; Sane defaults ;;;;;;;;;;;;;;;;;;;;;;;
(global-font-lock-mode t)    ; Enable syntax highlighting for older emacs
(global-auto-revert-mode 1)  ; revert buffers for changed files

  ;;; window layout undo/redo
(setq winner-boring-buffers '("*Completions*" "*Compile-Log*" "*inferior-lisp*"
                              "*Fuzzy Completions*" "*Apropos*" "*Help*" "*cvs*"
                              "*Buffer List*" "*Ibuffer*" "*esh command on file*"))
(winner-mode 1)

;;; UTF-8 please
(setq locale-coding-system 'utf-8)    ; pretty
(set-terminal-coding-system 'utf-8)   ; pretty
(set-keyboard-coding-system 'utf-8)   ; pretty
(set-selection-coding-system 'utf-8)  ; please
(prefer-coding-system 'utf-8)         ; with sugar on top

;;; Show tab characters
;; (global-whitespace-mode 1)
(setq whitespace-style '(trailing face tabs tab-mark)  ; needs to be re-set in every buffer
      whitespace-display-mappings
      '((tab-mark   ?\t   [?| ?\t] [?\\ ?\t])
        (newline-mark 10 [36 10])))                    ; for whitespace-newline-mode

;; avoid garbage collection (default is 400k)
(setq-default
      gc-cons-threshold 20000000
      confirm-kill-emacs nil
      ;; minibufferception? Yay!
      enable-recursive-minibuffers t)

;; Show me those keystrokes
(setq echo-keystrokes 0.02

      ring-bell-function 'ignore

      inhibit-startup-screen t                ; don't show EMACs start screen
      inhibit-splash-screen t
      ;; inhibit-startup-message t
      inhibit-startup-echo-area-message "hlissner"
      ;; inhibit-startup-buffer-menu t

      initial-major-mode 'text-mode           ; initial scratch buffer mode
      initial-scratch-message nil

      compilation-always-kill t
      compilation-ask-about-save nil
      compilation-scroll-output t

      sentence-end-double-space nil           ; sentences end with periods. Period.

      ediff-diff-options "-w"
      ediff-split-window-function 'split-window-horizontally   ; side-by-side diffs
      ediff-window-setup-function 'ediff-setup-windows-plain)  ; no extra frames

;; Don't save clipboard contents into kill-ring before replacing them
(setq save-interprogram-paste-before-kill nil)

;; don't let the cursor go into minibuffer prompt
;; Tip taken from Xah Lee: http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

;; remove annoying ellipsis when printing sexp in message buffer
(setq eval-expression-print-length nil
      eval-expression-print-level nil)

;; Save cursor location across sessions. Only save for files that exist.
(use-package saveplace
  :init   (defvar save-place-file (concat TMP-DIR "saveplace"))
  :config (add-hook! 'find-file-hook (if (file-exists-p (buffer-file-name)) (setq save-place t))))

;; Save history across sessions
(use-package savehist
  :config
  (progn
    (setq savehist-file (concat TMP-DIR "savehist")  ; keep the home clean
          history-length 1000
          savehist-additional-variables '(kill-ring
                                          global-mark-ring
                                          search-ring
                                          regexp-search-ring
                                          extended-command-history))
    (savehist-mode 1)))

(use-package recentf
  :config
  (progn
    (setq recentf-save-file (concat TMP-DIR "recentf")
          recentf-exclude '("/tmp/" "/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$"
                            "/\\.cache/.+" "emacs\\.d/workgroups/.+$" ".emacs.workgroup"
                            "/company-statistics-cache.el$")
          recentf-max-menu-items 0
          recentf-max-saved-items 250
          recentf-auto-cleanup 600)
    (recentf-mode 1)))


;;;; Backup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Disable all backups (that's what git/dropbox are for)
(setq bookmark-save-flag        t
      bookmark-default-file     (concat TMP-DIR "bookmarks")
      auto-save-default         nil
      auto-save-list-file-name  (concat TMP-DIR-AUTOSAVE "auto-save")
      auto-save-file-name-transforms `((".*" ,TMP-DIR-AUTOSAVE t))
      ;; In case I want to reactivate backup files
      make-backup-files         nil
      create-lockfiles          nil
      backup-directory-alist   `((".*" . ,TMP-DIR-BACKUP)))


;;;; Undo Tree ;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default ; Remember undo history
 undo-tree-auto-save-history t
 undo-tree-history-directory-alist `(("." . ,TMP-DIR-UNDO)))

;; Keep region when undoing in region
(defadvice undo-tree-undo (around keep-region activate)
  (if (use-region-p)
      (let ((m (set-marker (make-marker) (mark)))
            (p (set-marker (make-marker) (point))))
        ad-do-it
        (goto-char p)
        (set-mark m)
        (set-marker p nil)
        (set-marker m nil))
    ad-do-it))

;; Shut up undo-tree's constant complaining: http://youtu.be/Z6woIRLnbmE
(defadvice undo-tree-load-history-hook (around undo-tree-load-history-shut-up activate) (shut-up ad-do-it))
(defadvice undo-tree-save-history-hook (around undo-tree-save-history-shut-up activate) (shut-up ad-do-it))
;; Silences an annoying error: undo-tree-mapc: Wrong type argument: listp, \.\.\.
(defadvice undo-tree-position (around undo-tree-position-silence-type-error activate)
  (when (listp (ad-get-args 1)) ad-do-it))

;;;; Editor behavior ;;;;;;;;;;;;;;;;
(setq-default ; spaces instead of tabs
 indent-tabs-mode nil
 tab-always-indent t
 tab-width 4)

(setq require-final-newline t
      delete-trailing-lines nil)

;; Automatic minor modes ;;;;;;;;;;;
(defvar narf/auto-minor-mode-alist ()
  "Alist of filename patterns vs correpsonding minor mode functions,
see `auto-mode-alist' All elements of this alist are checked, meaning
you can enable multiple minor modes for the same regexp.")
(defun narf|enable-minor-mode-maybe ()
  "Check file name against `narf/auto-minor-mode-alist'."
  (when buffer-file-name
    (let ((name buffer-file-name)
          (remote-id (file-remote-p buffer-file-name))
          (alist narf/auto-minor-mode-alist))
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
(add-hook 'find-file-hook 'narf|enable-minor-mode-maybe)


;;;; Utility plugins ;;;;;;;;;;;;;;;;;;
(use-package smex
  :functions (smex-initialize smex-update)
  :commands (smex smex-major-mode-commands)
  :config
  (progn
    (setq smex-save-file (expand-file-name "smex-items" TMP-DIR))
    (smex-initialize)
    ;; Hook up smex to auto-update, rather than update on every run
    (defun smex-update-after-load (unused)
      (when (boundp 'smex-cache) (smex-update)))
    (add-hook 'after-load-functions 'smex-update-after-load)))

(use-package popwin
  :config
  (progn ; popwin config
    (popwin-mode 1)
    (setq popwin:popup-window-height 0.45
          popwin:special-display-config
          (append '(("\\`\\*helm.*?\\*\\'" :regexp t :position bottom :height 15)
                    ("^\\*Flycheck.*\\*$" :regexp t :position bottom :height 0.25 :noselect t)
                    (inf-enh-ruby-mode :position bottom :stick t)
                    (snippet-mode :position bottom :stick t)
                    ("^\\*eclim.*\\*" :regexp t :position bottom :height 0.25)
                    ("*ansi-term*" :position bottom :height 0.45 :stick t)
                    ("*terminal*" :position bottom :height 0.45 :stick t)
                    ("*Async Shell Command*" :position bottom)
                    ("*Shell Command Output*" :position bottom :stick t :height 15)
                    ("* Regexp Explain *" :position top :height 0.35)
                    ("*anaconda-doc*" :position bottom :height 15 :noselect t)
                    ("*anaconda-nav*" :position bottom :height 15 :stick t)
                    ("^\\*Python.+\\*$" :regexp t :position bottom :height 20 :noselect t)
                    ("*Pp Eval Output*" :position bottom :height 10 :noselect t)
                    ("*eval*" :position bottom :noselect t)
                    (help-mode :height 25 :position bottom :stick t)
                    (compilation-mode :height 0.5 :position bottom :noselect t)
                    (diff-mode :position bottom :stick t)
                    ("*Backtrace*")
                    ("*Warnings*")
                    ("*Process List*")
                    ("*Compile-Log*" :height 0.3 :position bottom :noselect t)
                    (" *undo-tree*" :width 0.3 :position right)
                    ("^\\*scratch\\*.*" :regexp t :stick t)
                    (image-mode))
                  popwin:special-display-config))

    (defun popwin:toggle-popup-window ()
      (interactive)
      (if (popwin:popup-window-live-p)
          (popwin:close-popup-window)
        (popwin:popup-last-buffer)))))

(use-package semantic
  :commands semantic-mode
  :init
  (progn
    (add-hook 'c-mode-common-hook 'semantic-mode)
    (defvar semanticdb-default-system-save-directory (concat TMP-DIR "semanticdb"))
    (defvar semanticdb-default-save-directory        (concat TMP-DIR "semanticdb")))
  :config
  (semantic-mode 1))

;; Improved help commands
(use-package help-fns+
  :commands (describe-buffer
             describe-command
             describe-file
             describe-keymap
             describe-option
             describe-option-of-type))

(use-package server
  :config (unless (server-running-p) (server-start)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cond (IS-MAC      (require 'core-osx))
      (IS-LINUX    (require 'core-linux))
      (IS-WINDOWS  (require 'core-windows)))

;; Performance checks
(add-hook! 'find-file-hook
  ;; If file is oversized...
  (when (> (buffer-size) (* 1024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)
    (visual-line-mode)))


(provide 'core)
;;; core.el ends here
