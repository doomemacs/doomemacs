(my/install-package 'yasnippet)

(use-package yasnippet
    :diminish (yas-minor-mode . " $")
    :mode (("emacs.+/snippets/" . snippet-mode))
    :pre-load (progn
        ;; Fix yasnippet keymaps so they only work in insert mode (why they
        ;; had to make this so complicated I don't know); must be defined
        ;; BEFORE we include yasnippet.
        (defvar yas-minor-mode-map
          (let ((map (make-sparse-keymap)))
            (evil-define-key 'insert map [(tab)]     'yas-expand)
            (evil-define-key 'insert map (kbd "TAB") 'yas-expand)
            (evil-define-key 'insert map "\C-c&\C-s" 'yas-insert-snippet)
            (evil-define-key 'insert map "\C-c&\C-n" 'yas-new-snippet)
            (evil-define-key 'insert map "\C-c&\C-v" 'yas-visit-snippet-file)
            map)))
    :config
    (progn
      ;; Only load personal snippets
      (setq yas-snippet-dirs `(,my/snippets-dir))

      (yas-reload-all))
    :init
    (progn
      (add-hook 'prog-mode-hook 'yas-minor-mode)
      (add-hook 'org-mode-hook 'yas-minor-mode)))

;;
(provide 'init-snippets)
