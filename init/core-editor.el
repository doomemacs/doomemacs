;;;; Editor behavior ;;;;;;;;;;;;;;;;
;; (electric-indent-mode +1) 			; auto-indent on RET
;; (defun disable-electric-indent-mode ()
;;   (set (make-local-variable 'electric-indent-mode) nil)
;;   (setq-local tab-always-indent t))

;; ;; Only enable electric-mode in programming languages
;; (add-hook 'text-mode-hook 'disable-electric-indent-mode)
;; (add-hook 'org-mode-hook 'disable-electric-indent-mode)
;; (add-hook 'markdown-mode-hook 'disable-electric-indent-mode)

;; (global-hl-line-mode +1)            ; highlight the line
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

;;;; Plugins ;;;;;;;;;;;;;;;;;;;;;;;;
(use-package deferred :ensure t :defer t)
(use-package helm :ensure t :defer t)
(use-package ediff :ensure t :defer t)

(use-package evil :ensure t
  :diminish undo-tree-mode
  :config
  (progn
    ;; (setq evil-want-C-i-jump t)
    ;; (setq evil-want-C-u-scroll t)

    (evil-mode 1)

    (use-package evil-leader :ensure t)
    (use-package evil-matchit :ensure t)
    (use-package evil-surround :ensure t)
    (use-package evil-numbers :ensure t)
    (use-package evil-exchange :ensure t)
    (use-package evil-space :ensure t)
    (use-package evil-visualstar :ensure t)
    (use-package evil-nerd-commenter :ensure t)
    (use-package evil-ex-registers)

    ;; To get evil-leader mappings to work in the messages buffer...
    (kill-buffer "*Messages*")

    (setq evil-leader/in-all-states t)

    (global-evil-leader-mode 1)
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

(use-package rainbow-mode :ensure t :defer t)
(use-package rainbow-delimiters :ensure t
  :commands rainbow-delimiters-mode
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package rotate-text :commands (rotate-word-at-point rotate-region))

;;;; Init plugins ;;;;;;;;;;;;;;;;;;;
(use-package autopair :ensure t
  :diminish autopair-mode
  :init
  (progn (autopair-global-mode)
         (setq autopair-blink nil)
         ;; disable blink-matching-paren
         (setq blink-matching-paren nil)))

(use-package anzu :ensure t
  :diminish anzu-mode
  :init (global-anzu-mode))

(use-package key-chord :ensure t
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

(use-package multiple-cursors :ensure t
  :commands (mc/mark-next-like-this mc/mark-previous-like-this mc/mark-all-like-this)
  :config
  (progn
    ;; I do it this way because hooking mc/keyboard-quit to insert mode's exit
    ;; hook breaks multiple-cursors!
    (defadvice keyboard-quit (around mc-and-keyboard-quit activate)
      (mc/keyboard-quit) ad-do-it)))

(use-package smex :ensure t
  :commands (smex smex-major-mode-commands)
  :config
  (progn (smex-initialize)
         ;; Hook up smex to auto-update, rather than update on every run
         (defun smex-update-after-load (unused)
           (when (boundp 'smex-cache) (smex-update)))
         (add-hook 'after-load-functions 'smex-update-after-load)))

(use-package recentf :ensure t
  :init
  (progn (recentf-mode 1)
         (add-to-list 'recentf-exclude "\\.ido\\.last\\'")
         (add-to-list 'recentf-exclude "\\.revive\\'")
         (setq recentf-max-menu-items 0)
         (setq recentf-auto-cleanup 'never)))

;;
(provide 'core-editor)
