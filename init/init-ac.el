(my/install-package 'auto-complete)

(defun my/ac-path-completion-enable()
  (add-to-list 'ac-sources 'ac-source-filename)
  (add-to-list 'ac-sources 'ac-source-files-in-current-dir))

(use-package auto-complete
  :diminish auto-complete-mode
  :init
  (progn (require 'auto-complete-config)

         ;; Use more vim-like keymappings
         (add-hook 'change-major-mode-hook
                   (lambda () (imap ac-mode-map (kbd "C-SPC") 'auto-complete)))

         ;; (setq ac-auto-start 2)
         ;; (setq ac-auto-show-menu nil     ; Suggestions box must be invoked manually (see core-keymaps.el)
         ;;       ac-use-menu-map t         ; Enable ac-menu-map map when menu is open
         ;;       ac-us-quick-help nil      ; Don't show tooltips unless invoked (see core-keymaps.el)
         ;;       ac-fuzzy-cursor-color nil)

         ;; Keep auto-complete quiet until it's needed
         (setq ac-auto-start nil)

         (ac-config-default)
         (ac-linum-workaround)         ; Fix line number flux bug
         ;; (ac-flyspell-workaround) 		; Compatibility with flyspell/make
         )
  :config
  (progn (add-to-list 'ac-dictionary-files "~/.emacs.d/ac-dict/global")
         (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

         (imap ac-mode-map (kbd "C-S-SPC") 'auto-complete)
         (define-key ac-completing-map (kbd "<tab>") 'ac-expand)
         (define-key ac-completing-map (kbd "C-n") 'ac-next)
         (define-key ac-completing-map (kbd "C-p") 'ac-previous)
         (define-key ac-completing-map (kbd "<F1>") 'ac-quick-help)
         (define-key ac-completing-map (kbd "ESC") 'ac-stop)
         (define-key ac-completing-map [return] nil)

         ;; Tell ido not to care about case
         (setq completion-ignore-case t)
         ))

;;
(provide 'init-ac)
