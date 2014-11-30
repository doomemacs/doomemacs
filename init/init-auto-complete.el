(provide 'init-auto-complete)

(setq tags-case-fold-search nil)

(use-package auto-complete
  :init
  (progn
    (require 'auto-complete-config)

    (setq ac-auto-start nil
          ac-auto-show-menu t        ; Suggestions box must be invoked manually (see core-keymaps.el)
          ac-use-menu-map t          ; Enable ac-menu-map map when menu is open
          ac-use-quick-help t        ; Don't show tooltips unless invoked (see core-keymaps.el)
          ac-use-fuzzy t
          ac-candidate-limit 25)
    (setq ac-comphist-file (concat *tmp-dir "ac-comphist.dat"))

    (setq-default ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))

    (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
    (add-hook 'css-mode-hook 'ac-css-mode-setup)
    (add-hook 'shell-script-mode-hook 'ac-add-files)
    ;; (add-hook 'auto-complete-mode-hook 'ac-common-setup)
    )
  :config
  (progn
    ;; Redefine this function so auto-complete is available [almost] everywhere
    (defun auto-complete-mode-maybe ()
      (unless (minibufferp (current-buffer))
        (auto-complete-mode 1)))

    (global-auto-complete-mode t)

    (add-to-list 'ac-dictionary-files (expand-file-name "global" *ac-dicts-dir))
    (add-to-list 'ac-dictionary-directories *ac-dicts-dir)

    ;; Tell ido not to care about case
    (setq completion-ignore-case t)

    (bind 'insert ac-mode-map
          (kbd "C-x C-k")   'ac-complete-dictionary
          (kbd "C-x C-f")   'ac-complete-filename
          (kbd "C-x C-]")   'ac-complete-etags
          (kbd "C-x s")     'ac-complete-ispell
          (kbd "C-x C-s")   'ac-complete-ispell-fuzzy
          (kbd "C-x C-y")   'ac-complete-yasnippet
          (kbd "C-x C-o")   'auto-complete
          (kbd "C-SPC")     'auto-complete)

    (bind ac-completing-map
            (kbd "<tab>")  'ac-complete
            (kbd "C-n")    'ac-next
            (kbd "C-p")    'ac-previous
            (kbd "<f1>")   'ac-quick-help
            (kbd "C-<f1>") 'ac-help
            (kbd "ESC")    'ac-stop
            (kbd "RET")    'ac-complete)

    (use-package ac-etags
      :commands (ac-complete-etags)
      :config (ac-etags-setup))

    (use-package ac-ispell
      :commands (ac-complete-ispell ac-complete-ispell-fuzzy)
      :config
      (progn (ac-ispell-setup)
             (setq ac-ispell-requires 1
                   ac-ispell-fuzzy-limit 25)))))
