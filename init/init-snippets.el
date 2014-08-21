(use-package yasnippet :ensure t
    :diminish (yas-minor-mode . " $")
    :mode (("emacs.+/snippets/" . snippet-mode))
    :pre-load (progn
        ;; Fix yasnippet keymaps so they only work in insert mode (why they
        ;; had to make this so complicated I don't know); must be defined
        ;; BEFORE we include yasnippet.
        (defvar yas-minor-mode-map
          (let ((map (make-sparse-keymap)))
            (imap map [(tab)]     'yas-expand)
            (imap map (kbd "TAB") 'yas-expand)
            map)))
    :config
    (progn
      ;; Only load personal snippets
      (setq yas-snippet-dirs `(,my/snippets-dir))
      (setq yas-prompt-functions '(yas-ido-prompt yas-no-prompt))

      (imap yas-minor-mode-map (kbd "C-c C-s") 'yas-insert-snippet)
      (define-key yas-minor-mode-map (kbd "C-c C-n") 'yas-new-snippet)
      (define-key yas-minor-mode-map (kbd "C-c C-v") 'yas-visit-snippet-file)

      (yas-reload-all))
    :init
    (progn
      (add-hook 'prog-mode-hook 'yas-minor-mode)
      (add-hook 'snippet-mode-hook 'yas-minor-mode)
      (add-hook 'markdown-mode-hook 'yas-minor-mode)
      (add-hook 'org-mode-hook 'yas-minor-mode)))

;;
(provide 'init-snippets)
