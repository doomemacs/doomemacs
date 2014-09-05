(provide 'core-editor)

;;;; Editor behavior ;;;;;;;;;;;;;;;;
(setq sentence-end-double-space nil)
(setq require-final-newline t)

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default tab-always-indent nil)

;;;; Modes 'n hooks ;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.plist\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("zsh\\(env\\|rc\\)?\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("z\\(profile\\|login\\|logout\\)?\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("zsh/" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.applescript$" . applescript-mode))

(add-hook 'text-mode-hook 'enable-hard-wrap)
(add-hook 'prog-mode-hook 'enable-comment-hard-wrap)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;; Evil-mode ;;;;;;;;;;;;;;;;;;;;;;;
(use-package evil
  :diminish undo-tree-mode
  :config
  (progn
    (evil-mode 1)

    (use-package evil-matchit)
    (use-package evil-surround)
    (use-package evil-numbers)
    (use-package evil-exchange)
    (use-package evil-space)
    (use-package evil-visualstar)
    (use-package evil-nerd-commenter)
    (use-package evil-ex-registers)

    (global-evil-matchit-mode  1)
    (global-evil-surround-mode 1)

    (evil-exchange-install)

    (evil-space-setup "t" ";" ",")    ; Repeat t with space
    (evil-space-setup "f" ";" ",")    ; Repeat f with space
    (evil-space-setup "T" "," ";")    ; Repeat T with space
    (evil-space-setup "F" "," ";")    ; Repeat F with space
    (evil-define-operator evil-destroy (beg end type register yank-handler)
      (evil-delete beg end type ?_ yank-handler))

    ;; Enable half-cursor blink when using ace-jump
    (defadvice evil-ace-jump-char-mode (around evil-ace-jump-char-mode-operator-mode activate)
      (evil-half-cursor) ad-do-it)
    (defadvice evil-ace-jump-word-mode (around evil-ace-jump-word-mode-operator-mode activate)
      (evil-half-cursor) ad-do-it)

    (evil-set-initial-state 'comint-mode 'insert)

    ;; Enable registers in ex-mode
    (define-key evil-ex-completion-map (kbd "C-r") #'evil-ex-paste-from-register)
    ))

;;;; Editing plugins ;;;;;;;;;;;;;;;;;;;
(use-package expand-region)

(use-package autopair
  :diminish autopair-mode
  :init
  (progn (autopair-global-mode)
         (setq autopair-blink nil)
         ;; disable blink-matching-paren
         (setq blink-matching-paren nil)))

(use-package anzu
  :diminish anzu-mode
  :init (global-anzu-mode))

(use-package multiple-cursors
  :commands (mc/mark-next-like-this mc/mark-previous-like-this mc/mark-all-like-this)
  :config
  (progn
    ;; I do it this way because hooking mc/keyboard-quit to insert mode's exit
    ;; hook breaks multiple-cursors!
    (defadvice keyboard-quit (around mc-and-keyboard-quit activate)
      (mc/keyboard-quit) ad-do-it)))

;;;; Utility plugins ;;;;;;;;;;;;;;;;;;
(use-package key-chord
  :init
  (progn (key-chord-mode 1)
         (setq key-chord-two-keys-delay 0.5)))

(use-package smex
  :commands (smex smex-major-mode-commands)
  :config
  (progn (smex-initialize)
         ;; Hook up smex to auto-update, rather than update on every run
         (defun smex-update-after-load (unused)
           (when (boundp 'smex-cache) (smex-update)))
         (add-hook 'after-load-functions 'smex-update-after-load)))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package recentf
  :init
  (progn (recentf-mode 1)
         (setq recentf-max-menu-items 0
               recentf-max-saved-items 75
               recent5-auto-cleanup 'never
               recentf-exclude '("/tmp/"
                                 "/ssh:"
                                 "\\.ido\\.last\\'"
                                 "\\.revive\\'"))))
