(require-package 'evil)
(evil-mode nil)

;; Has to be done this way to ensure special buffers have evil,
;; evil-leader, and all the various keymaps enabled.
(add-hook 'after-init-hook (lambda() (evil-mode 1)))

;; Now we can carry on with the rest...
(require-packages
 '(evil-leader
   evil-nerd-commenter     ; auto commenting made easy
   evil-matchit            ; jumping between block delimiters
   evil-surround           ; surround-with delimiters
   evil-numbers            ; increment/decrement numbers
   evil-exchange           ; exchanging two text objects (gx/gX)
   evil-space              ; mimics ; and , for f, F, t, T w/ space
   evil-visualstar         ; visual-based * and #
   evil-ex-registers       ; paste from registers in ex commands
   autopair                ; delimiter auto-closing
   rainbow-delimiters      ; colored matching parenthesis
   rainbow-mode            ; highlight color codes
   saveplace               ; restore cursor position on buffer load
   volatile-highlights     ; temporarily highlight changes on undo/yank
   anzu                    ; display current + total matches searching
   smex                    ; less M-x cruft
   rotate-text             ; like vim-switch
   recentf                 ; access to list of recent files
   key-chord               ; for mapping key chords in insert mode
   multiple-cursors		   ; cursors, of the numerous variety
   ediff
   ))


;;;; Editor behavior ;;;;;;;;;;;;;;;;

(setq initial-scratch-buffer nil)   ; empty scratch buffer
(kill-buffer "*scratch*")

(electric-indent-mode +1) 			; auto-indent on RET
(global-hl-line-mode +1)            ; highlight the line
(setq-default
  tab-width             4           ; set tab width to 4 for all buffers
  indent-tabs-mode      t           ; always replace tabs with spaces
  tab-always-indent     nil)

;; do not soft-wrap lines
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)

;; Prettify code-related major modes
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Remove trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;;;; Init plugins ;;;;;;;;;;;;;;;;;;;

(diminish 'undo-tree-mode)

;;;#evil
(setq evil-want-C-i-jump t)
(setq evil-want-C-u-scroll t)
(setq evil-leader/in-all-states t)

(global-evil-leader-mode)
(global-evil-matchit-mode  1)
(global-evil-surround-mode 1)

(evil-exchange-install)

(evil-space-setup "t" ";" ",")		; Repeat t with space
(evil-space-setup "f" ";" ",")		; Repeat f with space
(evil-space-setup "T" "," ";")		; Repeat T with space
(evil-space-setup "F" "," ";")		; Repeat F with space
(evil-define-operator evil-destroy (beg end type register yank-handler)
    (evil-delete beg end type ?_ yank-handler))

;; Enable registers in ex-mode
(define-key evil-ex-completion-map (kbd "C-r") #'evil-ex-paste-from-register)

;;;#autopair
(autopair-global-mode)
(setq autopair-blink nil)
(setq blink-matching-paren nil)     ; disable blink-matching-paren
(diminish 'autopair-mode)

;;;#anzu
(global-anzu-mode)
(diminish 'anzu-mode)

;;;#ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;;#key-chord
(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.5)

;;;#volatile-highlights
(volatile-highlights-mode t)
(diminish 'volatile-highlights-mode)

;;;#saveplace
(setq-default save-place t)
(setq save-place-file (expand-file-name "saveplace" my-tmp-dir))

;;;#savehist
(setq savehist-additional-variables
      ;; search entries
      '(search ring regexp-search-ring)
      ;; save every 5 minutes
      savehist-autosave-interval 300
      ;; keep the home clean
      savehist-file (expand-file-name "savehist" my-tmp-dir))
(savehist-mode 1)

;;;#smex
(smex-initialize)
;; Hook up smex to auto-update, rather than update on every run
(defun smex-update-after-load (unused)
  (when (boundp 'smex-cache) (smex-update)))
(add-hook 'after-load-functions 'smex-update-after-load)

;;;#recentf
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(add-to-list 'recentf-exclude "\\.ido\\.last\\'")
(add-to-list 'recentf-exclude "\\.revive\\'")
(setq recentf-auto-cleanup 'never)


;;
(provide 'core-editor)
