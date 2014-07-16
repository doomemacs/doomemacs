(require-package 'evil)
(evil-mode 1)

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
    auto-complete           ; self-explanity
    auto-complete-config    ; its default config
    fuzzy                   ; fuzzy search engine for auto-complete
    autopair                ; delimiter auto-closing
    yasnippet               ; snippets: expand with tab
    rainbow-delimiters      ; colored matching parenthesis
    rainbow-mode            ; highlight color codes
    highlight-indentation   ; visual indentation guides
    diminish                ; shrinks/removes modeline elements
    saveplace               ; restore cursor position on buffer load
    volatile-highlights     ; temporarily highlight changes on undo/yank
    anzu                    ; display current + total matches searching
    smex                    ; less M-x cruft
    rotate-text             ; like vim-switch
    uniquify                ; unique buffer names for identical filenames
    recentf                 ; access to list of recent files
    key-chord               ; for mapping key chords in insert mode
    ediff
    ))


;;;; Editor behavior ;;;;;;;;;;;;;;;;

;;;#key-chord
(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.5)

(electric-indent-mode +1)
(global-hl-line-mode +1)            ; highlight the line
(setq blink-matching-paren nil)     ; disable blink-matching-paren
(setq-default
  tab-width             4           ; set tab width to 4 for all buffers
  indent-tabs-mode      nil         ; always replace tabs with spaces
  tab-always-indent     nil)

;; do not soft-wrap lines
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)

;; Prettify code-related major modes
(add-hook 'prog-mode-hook 'linum-on)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'highlight-indentation-mode)

;; Remove trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;#smex
(smex-initialize)
;; Hook up smex to auto-update, rather than update on every run
(defun smex-update-after-load (unused)
  (when (boundp 'smex-cache) (smex-update)))
(add-hook 'after-load-functions 'smex-update-after-load)


;;;; Init plugins ;;;;;;;;;;;;;;;;;;;

;;;#evil
(setq evil-want-C-i-jump t)
(setq evil-want-C-u-scroll t)

(global-evil-leader-mode)
(global-evil-matchit-mode  1)
(global-evil-surround-mode 1)

(evil-exchange-install)

(evil-space-setup "t" ";" ",")
(evil-space-setup "f" ";" ",")
(evil-space-setup "T" "," ";")
(evil-space-setup "F" "," ";")
(evil-define-operator evil-destroy (beg end type register yank-handler)
    (evil-delete beg end type ?_ yank-handler))

;; Enable registers in ex-mode
(define-key evil-ex-completion-map (kbd "C-r") #'evil-ex-paste-from-register)


;;;#autopair
(autopair-global-mode)
(diminish 'autopair-mode)


;;;#anzu
(global-anzu-mode)
(diminish 'anzu-mode)


;;;#ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)


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


;;;#diminish
(diminish 'undo-tree-mode)
(diminish 'highlight-indentation-mode)



;;;#uniquify
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers


;;;#recentf
(recentf-mode 1)
(setq recentf-max-menu-items 25)


;;;#yasnippet
(yas-global-mode -1)



;;;; Auto-completion ;;;;;;;;;;;;;;

(ac-config-default)
(ac-linum-workaround)         ; Fix line number flux bug
(diminish 'auto-complete-mode)

(add-hook 'prog-mode-hook 'enable-path-completion)
(setq ac-auto-show-menu nil     ; Suggestions box must be invoked manually (see core-keymaps.el)
      ac-use-menu-map t         ; Enable ac-menu-map map when menu is open
      ac-us-quick-help nil)     ; Don't show tooltips unless invoked (see core-keymaps.el)

(defun enable-path-completion ()
      (add-to-list 'ac-sources 'ac-source-filename)
      (add-to-list 'ac-sources 'ac-source-files-in-current-dir))

;; Tell ido not to care about case
(setq completion-ignore-case t)

;;; Filters ido-matches setting acronynm matches in front of the results
(defadvice ido-set-matches-1 (after ido-smex-acronym-matches activate)
  (if (and (fboundp 'smex-already-running) (smex-already-running)
           (> (length ido-text) 1))
      (let ((regex (concat "^" (mapconcat 'char-to-string ido-text "[^-]*-")))
            (acronym-matches (list))
            (remove-regexes '("-menu-")))
        ;; Creating the list of the results to be set as first
        (dolist (item items)
          (if (string-match (concat regex "[^-]*$") item) ;; strict match
              (add-to-list 'acronym-matches item)
            (if (string-match regex item) ;; appending relaxed match
                (add-to-list 'acronym-matches item t))))

        ;; Filtering ad-return-value
        (dolist (to_remove remove-regexes)
          (setq ad-return-value
                (delete-if (lambda (item)
                             (string-match to_remove item))
                           ad-return-value)))

        ;; Creating resulting list
        (setq ad-return-value
              (append acronym-matches
                      ad-return-value))

        (delete-dups ad-return-value)
        (reverse ad-return-value))))


;;
(provide 'core-editor)
