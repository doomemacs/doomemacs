;;;; Editor behavior ;;;;;;;;;;;;;;;;
(blink-cursor-mode -1)

(setq-default
  tab-width             4           ; set tab width to 4 for all buffers
  indent-tabs-mode      nil         ; use tabs, not spaces
  tab-always-indent     nil)
;; do not soft-wrap lines
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)
;; Remove trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; All this just to show errant tab characters
(add-hook 'font-lock-mode-hook
 (function
  (lambda ()
    (setq font-lock-keywords
     (append font-lock-keywords
      '(("\r" (0 'my-carriage-return-face t))
        ("\t" (0 'my-tab-face t))))))))
(setq whitespace-style (quote (face trailing tab-mark)))
(setq whitespace-display-mappings '((tab-mark 9 [?> 9] [92 9])))
(add-hook 'find-file-hook 'whitespace-mode)

;;;; Plugins ;;;;;;;;;;;;;;;;;;;;;;;;
(use-package highlight-indentation
  :init
  (add-hook 'prog-mode-hook 'highlight-indentation-mode))

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

    ;; To get evil-leader mappings to work in the messages buffer...
    (kill-buffer "*Messages*")

    (global-evil-matchit-mode  1)
    (global-evil-surround-mode 1)

    (evil-exchange-install)

    (evil-space-setup "t" ";" ",")    ; Repeat t with space
    (evil-space-setup "f" ";" ",")    ; Repeat f with space
    (evil-space-setup "T" "," ";")    ; Repeat T with space
    (evil-space-setup "F" "," ";")    ; Repeat F with space
    (evil-define-operator evil-destroy (beg end type register yank-handler)
      (evil-delete beg end type ?_ yank-handler))

    (evil-set-initial-state 'comint-mode 'insert)

    ;; Enable registers in ex-mode
    (define-key evil-ex-completion-map (kbd "C-r") #'evil-ex-paste-from-register)))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package rotate-text
  :commands (rotate-word-at-point rotate-region))

;;;; Init plugins ;;;;;;;;;;;;;;;;;;;
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

(use-package expand-region)

(use-package key-chord
  :init
  (progn (key-chord-mode 1)
         (setq key-chord-two-keys-delay 0.5)))

(use-package saveplace
  :init
  (progn (setq-default save-place t)
         (setq save-place-file (expand-file-name "saveplace" my/tmp-dir))))

(use-package savehist
  :init
  (progn (setq savehist-additional-variables
               ;; search entries
               '(search ring regexp-search-ring)
               ;; save every 5 minutes
               savehist-autosave-interval 300
               ;; keep the home clean
               savehist-file (expand-file-name "savehist" my/tmp-dir))
         (savehist-mode 1)))

(use-package multiple-cursors
  :commands (mc/mark-next-like-this mc/mark-previous-like-this mc/mark-all-like-this)
  :config
  (progn
    ;; I do it this way because hooking mc/keyboard-quit to insert mode's exit
    ;; hook breaks multiple-cursors!
    (defadvice keyboard-quit (around mc-and-keyboard-quit activate)
      (mc/keyboard-quit) ad-do-it)))

(use-package smex
  :commands (smex smex-major-mode-commands)
  :config
  (progn (smex-initialize)
         ;; Hook up smex to auto-update, rather than update on every run
         (defun smex-update-after-load (unused)
           (when (boundp 'smex-cache) (smex-update)))
         (add-hook 'after-load-functions 'smex-update-after-load)))

(use-package recentf
  :init
  (progn (recentf-mode 1)
         (add-to-list 'recentf-exclude "\\.ido\\.last\\'")
         (add-to-list 'recentf-exclude "\\.revive\\'")
         (setq recentf-max-menu-items 0)
         (setq recentf-auto-cleanup 'never)))

;;
(provide 'core-editor)
