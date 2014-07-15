(require 'cl)

;; Emacs under-the-hood
(prefer-coding-system 'utf-8)
(setq-default gc-cons-threshold 50000000) ; avoid garbage collection (default is only 400k)
(setq make-backup-files         nil)      ; Don't want any backup files
(setq auto-save-list-file-name  nil)      ; Don't want any .saves files
(setq auto-save-default         nil)      ; Don't want any auto saving

;; If I ever enable bkacups/autosaves, then change where they go
(setq backup-directory-alist `((".*" . ,my-tmp-dir)))
(setq auto-save-file-name-transforms `((".*" ,my-tmp-dir t)))

;; Always revert buffers if the files were changed
(global-auto-revert-mode 1)

; window layout undo/redo, keymaps in init-evil.el
(when (fboundp 'winner-mode) (winner-mode 1))
(setq linum-delay t)

;;;; My personal minor mode ;;;;;;;;

(defvar my-mode-map (make-sparse-keymap))
(define-minor-mode my-mode :keymap my-mode-map)


;;;; Commands ;;;;;;;;;;;;;;;;;;;;;;

(defun minibuffer-quit ()
  "Abort recursive edit.
        In Delete Selection mode, if the mark is active, just deactivate it;
        then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

;; File navigation defuns
(defun my-conf-edit ()
  (interactive)
  (find-file (expand-file-name "init.el" my-dir)))

(defun my-conf-find ()
  (interactive)
  (projectile-find-file-in-directory my-dir))

;;
(defun copy-to-end-of-line ()
  (interactive)
  (evil-yank (point) (point-at-eol)))

(defun backward-kill-line ()
  (interactive)
  (evil-delete (point-at-bol) (point)))

(defun toggle-sidebar ()
  (interactive)
  (sr-speedbar-toggle)
  (sr-speedbar-refresh-turn-off))


;;;; Macros ;;;;;;;;;;;;;;;;;;;;;;;;

;; Vimmish keymapping shortcuts
(defmacro gmap (key command)   `(global-set-key ,key ,command))
(defmacro nmap (key command)   `(define-key evil-normal-state-map ,key ,command))
(defmacro vmap (key command)   `(define-key evil-visual-state-map ,key ,command))
(defmacro imap (key command)   `(define-key evil-insert-state-map ,key ,command))
(defmacro ichmap (key command) `(key-chord-define evil-insert-state-map ,key ,command))
(defmacro cmap (ex function)   `(evil-ex-define-cmd ,ex ,function))

;; This one's unique for my own special mappings
(defmacro map (key command)
  `(define-key my-mode-map ,key ,command))
(defmacro emap (mode key command)
  `(evil-define-key ,mode my-mode-map ,key ,command))

(defmacro is-osx () '(eq system-type 'darwin))


;;;; Defuns ;;;;;;;;;;;;;;;;;;;;;;;

(defun linum-on () (linum-mode 0))
(defun linum-off () (linum-mode 0))


;;
(provide 'core)
