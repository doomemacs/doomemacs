;; Setup theme
(add-to-list 'custom-theme-load-path my/themes-dir)
(load-theme my/theme t)

;; Emacs under-the-hood
(prefer-coding-system 'utf-8)
(setq redisplay-dont-pause t)
(setq-default gc-cons-threshold 50000000) ; avoid garbage collection (default is 400k)
(setq make-backup-files         nil       ; Don't want any backup files
      auto-save-list-file-name  nil       ; Don't want any .saves files
      auto-save-default         nil)      ; Don't want any auto saving
(fset 'yes-or-no-p 'y-or-n-p)       ; y/n instead of yes/no
(setq inhibit-startup-screen t)     ; don't show EMACs start screen

;; If I ever enable bkacups/autosaves, then change where they go
(setq backup-directory-alist `((".*" . ,my/tmp-dir)))
(setq auto-save-file-name-transforms `((".*" ,my/tmp-dir t)))

;; Always revert buffers if the files were changed
(global-auto-revert-mode 1)

;; window layout undo/redo, keymaps in core-keymaps.el
(when (fboundp 'winner-mode) (winner-mode 1))

(defconst is-mac
  (eq system-type 'darwin)
  "Is this running on OS X?")
(defconst is-linux
  (eq system-type 'gnu/linux)
  "Is this running on Linux?")

;;;; Macros ;;;;;;;;;;;;;;;;;;;;;;;;

;; Vimmish keymapping shortcuts
(defmacro nmap (map &rest body)
  `(evil-define-key 'normal ,map ,@body))
(defmacro vmap (map &rest body)
  `(evil-define-key 'visual ,map ,@body))
(defmacro imap (map &rest body)
  `(evil-define-key 'insert ,map ,@body))
(defmacro emap (map &rest body)
  `(evil-define-key 'emacs ,map ,@body))

;; insert-mode key-chord mapping
(defmacro ichmap (key command)
  `(key-chord-define evil-insert-state-map ,key ,command))


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

;; Prevent prompts when trying to close window. If I'm closing the window,
;; I likely want it to close!
(when window-system
  (defadvice save-buffers-kill-emacs (around no-y-or-n activate)
    (flet ((yes-or-no-p (&rest args) t)
           (y-or-n-p (&rest args) t))
      ad-do-it)))

;; Prevent GUI dialog boxes, they make emacs hang
(defadvice yes-or-no-p (around prevent-dialog activate)
  (let ((use-dialog-box nil)) ad-do-it))
(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  (let ((use-dialog-box nil)) ad-do-it))


;;;; My personal minor mode ;;;;;;;;

(defvar my/mode-map (make-sparse-keymap))
(define-minor-mode my/mode :keymap my/mode-map :global t)


;;;; Commands ;;;;;;;;;;;;;;;;;;;;;;

;; File navigation defuns
(defun my/initfiles ()
  (interactive)
  (ido-find-file-in-dir my/dir))

(defun my/open-scratch ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (text-mode))

(defun my/expand-space ()
  (interactive)
  (save-excursion (insert " ")))

(defun my/expand-backspace ()
  (interactive)
  (save-excursion (delete-char 1))
  (delete-backward-char 1))

(defun my/enable-hard-wrap()
  (interactive)
  (auto-fill-mode 1))

(defun my/byte-recompile ()
  (interactive)
  (byte-recompile-file (expand-file-name "init.el" my/dir))
  (byte-recompile-directory my/init-dir 0)
  (byte-recompile-directory my/elisp-dir 0))


;;;; Load the rest ;;;;;;;;;;;;;;;;;;

(require 'core-packages)
(require 'core-ui)
(require 'core-editor)
(use-package core-osx :if is-mac)
(add-hook 'after-init-hook (lambda() (require 'core-keymaps)))

;;
(provide 'core)
