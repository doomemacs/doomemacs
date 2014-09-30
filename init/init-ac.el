(provide 'init-ac)

;;
(use-package auto-complete
  :diminish auto-complete-mode
  :init
  (progn
    (require 'auto-complete-config)

    (setq ac-auto-start t
          ac-auto-show-menu t        ; Suggestions box must be invoked manually (see core-keymaps.el)
          ac-use-menu-map t          ; Enable ac-menu-map map when menu is open
          ac-use-quick-help t        ; Don't show tooltips unless invoked (see core-keymaps.el)
          ac-use-fuzzy nil
          ac-candidate-limit 25)
    (setq ac-comphist-file (concat *tmp-dir "ac-comphist.dat"))

    (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
    (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
    (add-hook 'css-mode-hook 'ac-css-mode-setup)
    (add-hook 'shell-script-mode-hook 'ac-add-files)
    ;; (add-hook 'auto-complete-mode-hook 'ac-common-setup)
    (global-auto-complete-mode t))
  :config
  (progn
    (add-to-list 'ac-dictionary-files (expand-file-name "global" *ac-dicts-dir))
    (add-to-list 'ac-dictionary-directories *ac-dicts-dir)
    (add-to-list 'ac-modes 'nxml-mode)

    ;; Tell ido not to care about case
    (setq completion-ignore-case t)))
