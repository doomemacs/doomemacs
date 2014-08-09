(use-package auto-complete :ensure t
  :diminish auto-complete-mode
  :init
  (progn
    (require 'auto-complete-config)

    (setq ac-auto-start nil)
    (setq ac-auto-show-menu t        ; Suggestions box must be invoked manually (see core-keymaps.el)
          ac-use-menu-map t          ; Enable ac-menu-map map when menu is open
          ac-use-quick-help nil       ; Don't show tooltips unless invoked (see core-keymaps.el)
          ac-use-fuzzy nil
          ac-candidate-limit 25)

    (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
    (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
    (add-hook 'css-mode-hook 'ac-css-mode-setup)
    (add-hook 'shell-script-mode-hook 'my/ac-files-setup)
    ;; (add-hook 'auto-complete-mode-hook 'ac-common-setup)
    (global-auto-complete-mode t)

    ;; Fix line number flux bug
    (ac-linum-workaround))
  :config
  (progn
    (add-to-list 'ac-dictionary-files "~/.emacs.d/ac-dict/global")
    (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

    (imap ac-mode-map (kbd "C-SPC") 'auto-complete)
    ;; (imap ac-mode-map (kbd "C-S-SPC") 'auto-complete)
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
