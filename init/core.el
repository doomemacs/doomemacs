(when (functionp 'scroll-bar-mode) (scroll-bar-mode -1))    ; no scrollbar
(when (functionp 'tool-bar-mode)   (tool-bar-mode -1))      ; no toolbar
(when (functionp 'menu-bar-mode)   (menu-bar-mode -1))      ; no menubar
(when (fboundp 'fringe-mode) (fringe-mode '(4 . 10)))       ; no nonsense

(defconst is-mac     (eq system-type 'darwin))
(defconst is-linux   (eq system-type 'gnu/linux))
(defconst is-windows (eq system-type 'windows-nt))

(setq use-package-verbose DEBUG-MODE)

(cd "~") ; instead of /

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'defuns)
(require 'autoloads) ; use make autoloads to generate autoloads file
(use-package shut-up
  :config
  (progn
    (setq shut-up-ignore DEBUG-MODE)
    (when noninteractive (shut-up-silence-emacs)))) ; http://youtu.be/Z6woIRLnbmE

;; Make sure undo/backup folders exist
(defconst my-tmp-dir-undo     (expand-file-name "undo" my-tmp-dir))
(defconst my-tmp-dir-backup   (expand-file-name "backup" my-tmp-dir))
(defconst my-tmp-dir-autosave (expand-file-name "autosave" my-tmp-dir))
(unless (file-directory-p my-tmp-dir)
   (make-directory my-tmp-dir-undo t)
   (make-directory my-tmp-dir-backup t)
   (make-directory my-tmp-dir-autosave t))

;; (setq load-prefer-newer t)
(setq debug-on-quit DEBUG-MODE)

  ;;;; Sane defaults ;;;;;;;;;;;;;;;;;;;;;;;
(line-number-mode 1)         ; hide line no in modeline
(column-number-mode 1)       ; hide col no in modeline
(auto-compression-mode t)    ; Transparently open compressed files
(global-font-lock-mode t)    ; Enable syntax highlighting for older emacs
(global-auto-revert-mode 1)  ; revert buffers for changed files

  ;;; window layout undo/redo
(setq winner-boring-buffers '("*Completions*" "*Compile-Log*" "*inferior-lisp*"
                              "*Fuzzy Completions*" "*Apropos*" "*Help*" "*cvs*"
                              "*Buffer List*" "*Ibuffer*" "*esh command on file*"))
(winner-mode 1)

(use-package semantic
  :commands semantic-mode
  :init (add-hook 'prog-mode-hook 'semantic-mode)
  :config
  (progn
    (semantic-mode 1)
    (setq semanticdb-default-save-directory (expand-file-name "semanticdb" my-tmp-dir))))

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

;; minibufferception? Yay!
(setq-default enable-recursive-minibuffers t)

;; Show me those keystrokes
(setq echo-keystrokes 0.02)

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

(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)  ; side-by-side diffs
(setq ediff-window-setup-function 'ediff-setup-windows-plain) ; no extra frames

;; Fixes C-i's synonymity with TAB
(keyboard-translate ?\C-i ?\H-i)

;; Don't save clipboard contents into kill-ring before replacing them
(setq save-interprogram-paste-before-kill nil)

;; don't let the cursor go into minibuffer prompt
;; Tip taken from Xah Lee: http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

;; remove annoying ellipsis when printing sexp in message buffer
(setq eval-expression-print-length nil
      eval-expression-print-level nil)

;; Save history across sessions
(use-package savehist
  :config
  (progn
    (setq savehist-file (concat my-tmp-dir "savehist")  ; keep the home clean
          history-length 1000
          savehist-additional-variables '(kill-ring
                                          global-mark-ring
                                          search-ring
                                          regexp-search-ring
                                          extended-command-history))
    (savehist-mode 1)))

;; Save cursor location across sessions
(use-package saveplace
  :config
  (progn
    (setq-default save-place-file (concat my-tmp-dir "saveplace"))
    ;; activate save-place only for files that exist
    (add-hook! 'find-file-hook (if (file-exists-p buffer-file-name) (setq save-place t)))))

(use-package recentf
  :config
  (progn
    (add-hook 'kill-emacs-hook 'recentf-cleanup)
    (setq recentf-save-file (concat my-tmp-dir "recentf")
          recentf-exclude '("/tmp/" "/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$" "/\\.cache/.+" "emacs\\.d/workgroups/.+$" ".emacs.workgroup")
          recentf-max-menu-items 0
          recentf-max-saved-items 1000
          recentf-auto-cleanup 'never)
    (recentf-mode 1)))


  ;;;; Backup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Disable all backups (that's what git/dropbox are for)
(setq bookmark-save-flag        t)
(setq bookmark-default-file     (expand-file-name "bookmarks" my-tmp-dir))
(setq auto-save-default         nil)
(setq auto-save-list-file-name  (expand-file-name ".auto-save" my-tmp-dir-autosave))
(setq auto-save-file-name-transforms `((".*" ,my-tmp-dir-autosave t)))
;; In case I want to reactivate backup files
(setq make-backup-files         nil)
(setq create-lockfiles          nil)
(setq backup-directory-alist `((".*" . ,my-tmp-dir-backup)))
;; Remember undo history
(setq-default undo-tree-auto-save-history t)
(setq-default undo-tree-history-directory-alist `(("." . ,my-tmp-dir-undo)))
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

;; Shut up undo-tree's constant complaining
(defadvice undo-tree-load-history-hook (around undo-tree-load-history-shut-up activate)
  (shut-up ad-do-it))
;; (defadvice undo-tree-save-history-hook (around undo-tree-save-history-shut-up activate)
;;   (shut-up ad-do-it))

;; Silences an annoying error:
;; undo-tree-mapc: Wrong type argument: listp, \.\.\.
(defun undo-tree-position (node list)
  (when (listp list)
    (let ((i 0))
      (catch 'found
        (while (progn
                 (when (eq node (car list)) (throw 'found i))
                 (incf i)
                 (setq list (cdr list))))
        nil))))

;; What we do every night, Pinkie...
(defun display-startup-echo-area-message ()
  (message "What're we gonna do tonight, Brain? (Loaded in %s)" (emacs-init-time)))


  ;;;; Editor behavior ;;;;;;;;;;;;;;;;
;; spaces instead of tabs
(setq-default indent-tabs-mode nil     ; spaces instead of tabs
              tab-always-indent t
              tab-width 4)

(setq require-final-newline t)
(setq delete-trailing-lines nil)
(add-hook! 'makefile-mode-hook (setq indent-tabs-mode t)) ; Use normal tabs in makefiles

;; Project defuns ;;;;;;;;;;;;;;;;;;;;;;
(require 'f)

(defvar project-root-files '(".git" ".hg" ".svn" ".project" "local.properties" "project.properties" "rebar.config" "project.clj" "SConstruct" "pom.xml" "build.sbt" "build.gradle" "Gemfile" "requirements.txt" "tox.ini" "package.json" "gulpfile.js" "Gruntfile.js" "bower.json" "composer.json" "Cargo.toml" "mix.exs")
  "A list of files that count as 'project files', which determine whether a
    folder is the root of a project or not.")
(defun project-root (&optional strict-p)
  "Get the path to the root of your project. Uses `project-root-files' to
determine if a directory is a project."
  (catch 'found
    (f-traverse-upwards
     (lambda (path)
       (let ((path (file-truename path))
             (home (file-truename "~")))
         (if (f-equal? home path)
             (throw 'found (if strict-p nil default-directory))
           (dolist (file project-root-files)
             (when (f-exists? (expand-file-name file path))
               (throw 'found path)))))) default-directory)
    default-directory))

(defun project-has-files (files &optional root)
  "Return non-nil if `file' exists in the project root."
  (let ((root (or root (project-root)))
        (files (if (listp files) files (list files)))
        found-p file)
    (while (and files (not found-p))
      (setq file (pop files))
      (setq found-p (f-exists? (project-path-to file root))))
    found-p))

(defun project-path-to (file &optional root)
  (let ((root (or root (project-root))))
    (expand-file-name file root)))

(defun project-name ()
  (file-name-nondirectory (directory-file-name (project-root))))

(defun project-p ()
  (not (null (project-root t))))

;; Make sure scratch buffer is always "in a project"
(defvar project-scratch-buffer nil)
(defun project-create-scratch-buffer ()
  (let* ((scratch-buffer (get-buffer-create "*scratch*"))
         (project-name (project-name))
         (root (project-root)))
    (mapc (lambda (b)
            (if (string-match-p "\\*scratch\\* (.+)" (buffer-name b))
                (kill-buffer b)))
          (buffer-list))
    (save-window-excursion
      (switch-to-buffer scratch-buffer)
      (setq project-scratch-buffer scratch-buffer)
      (erase-buffer)
      (cd root)
      (rename-buffer (format "*scratch* (%s)" project-name)))))
(add-hook 'find-file-hook 'project-create-scratch-buffer)


;; Automatic minor modes ;;;;;;;;;;;
(defvar auto-minor-mode-alist ()
  "Alist of filename patterns vs correpsonding minor mode functions,
see `auto-mode-alist' All elements of this alist are checked, meaning
you can enable multiple minor modes for the same regexp.")
(defun enable-minor-mode-based-on-path ()
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
(add-hook 'find-file-hook 'enable-minor-mode-based-on-path)


;;;; Utility plugins ;;;;;;;;;;;;;;;;;;

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
    ;; (setq display-buffer-function 'popwin:display-buffer)

    (push '("\\`\\*helm.*?\\*\\'" :regexp t :position bottom :height 15) popwin:special-display-config)

    (push '("^\\*Flycheck.*\\*$" :regexp t :position bottom :height 0.25 :noselect t) popwin:special-display-config)
    (push '(inf-enh-ruby-mode :position bottom :stick t) popwin:special-display-config)
    (push '(snippet-mode :position bottom :stick t) popwin:special-display-config)
    (push '("^\\*eclim.*\\*" :regexp t :position bottom :height 0.25) popwin:special-display-config)

    (push '("*ansi-term*" :position bottom :height 0.45 :stick t) popwin:special-display-config)
    (push '("*terminal*" :position bottom :height 0.45 :stick t) popwin:special-display-config)
    (push '("*Async Shell Command*" :position bottom) popwin:special-display-config)
    (push '("*Shell Command Output*" :position bottom :stick t :height 15) popwin:special-display-config)

    (push '("* Regexp Explain *" :position top :height 0.35) popwin:special-display-config)

    (push '("*anaconda-doc*" :position bottom :height 15 :noselect t) popwin:special-display-config)
    (push '("*anaconda-nav*" :position bottom :height 15 :stick t) popwin:special-display-config)
    (push '("^\\*Python.+\\*$" :regexp t :position bottom :height 20 :noselect t) popwin:special-display-config)

    (push '(help-mode :height 0.5 :position bottom :stick t) popwin:special-display-config)
    (push '(compilation-mode :height 0.5 :position bottom :noselect t) popwin:special-display-config)
    (push '(diff-mode :position bottom :stick t) popwin:special-display-config)
    (push '("*Backtrace*") popwin:special-display-config)
    (push '("*Warnings*") popwin:special-display-config)
    (push '("*Process List*") popwin:special-display-config)
    (push '("*Compile-Log*" :height 0.3 :position bottom :noselect t) popwin:special-display-config)
    (push '(" *undo-tree*" :width 0.3 :position right) popwin:special-display-config)
    (push '("^\\*scratch\\*.*" :regexp t :stick t) popwin:special-display-config)
    (push '(image-mode) popwin:special-display-config)

    (push '("*NeoTree*" :position left :width 22 :stick t) popwin:special-display-config)

    (defun popwin:toggle-popup-window ()
      (interactive)
      (if (popwin:popup-window-live-p)
          (popwin:close-popup-window)
        (popwin:popup-last-buffer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cond (is-mac      (require 'core-osx))
      (is-linux    (require 'core-linux))
      (is-windows  (require 'core-windows)))

;; Performance checks
(add-hook! 'find-file-hook
  ;; If file is oversized...
  (when (> (buffer-size) (* 1024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)
    (visual-line-mode)))

(use-package server
  :config (unless (server-running-p) (server-start)))


(provide 'core)
;;; core.el ends here
