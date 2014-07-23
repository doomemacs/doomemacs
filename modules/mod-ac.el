(require-packages
 '(auto-complete           ; self-explanity
   auto-complete-config    ; its default config
   ))

;; (setq ac-auto-show-menu nil     ; Suggestions box must be invoked manually (see core-keymaps.el)
;;       ac-use-menu-map t         ; Enable ac-menu-map map when menu is open
;;       ac-us-quick-help nil      ; Don't show tooltips unless invoked (see core-keymaps.el)
;;       ac-fuzzy-cursor-color nil)

;; Keep auto-complete quiet until it's needed (see core-keymaps.el)
(setq ac-auto-start nil)

(ac-config-default)
(ac-linum-workaround)         	; Fix line number flux bug
(ac-flyspell-workaround) 		; Compatibility with flyspell/make
(diminish 'auto-complete-mode)	; Hide mode-line entry

(add-to-list 'ac-dictionary-files "~/.emacs.d/ac-dict/global")
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

;; Use more vim-like keymappings
(evil-define-key 'insert ac-mode-map (kbd "C-SPC") 'auto-complete)
(evil-define-key 'insert ac-mode-map (kbd "C-S-SPC") 'ac-fuzzy-complete)
(define-key ac-completing-map (kbd "<tab>") 'ac-next)
(define-key ac-completing-map (kbd "S-<tab>") 'ac-previous)
(define-key ac-completing-map (kbd "<F1>") 'ac-quick-help)
(define-key ac-completing-map [return] 'ac-complete)
(define-key ac-completing-map (kbd "ESC") 'ac-stop)

(add-hook 'prog-mode-hook 'enable-path-completion)

(defun enable-path-completion ()
  (add-to-list 'ac-sources 'ac-source-filename)
  (add-to-list 'ac-sources 'ac-source-files-in-current-dir))

;; Tell ido not to care about case
(setq completion-ignore-case t)

;;
(provide 'mod-ac)
