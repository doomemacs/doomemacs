(use-package yaml-mode :ensure t :mode "\\.yaml\\'")
(use-package json-mode :ensure t
  :mode (("\\.json\\'" . json-mode)
         ("\\.jshintrc\\'" . json-mode)))

(use-package python :ensure python-mode
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (progn
    (use-package jedi :ensure t
      :init
      (if (not (file-directory-p "~/.emacs.d/.python-environments/default/"))
          (jedi:install-server)))

    ;; Let autopair work with triple-quotes
    (setq autopair-handle-action-fns
          (list #'autopair-default-handle-action
                #'autopair-python-triple-quote-action))

    ;; Don't remap backspace. Leave it to autopair, please.
    (define-key python-mode-map [backspace] nil)
    (my/setup-run-code python-mode-map "python"))
  :init
  (add-hook 'python-mode-hook 'jedi:ac-setup)
  ;; (evil-define-key 'insert ac-mode-map (kbd "C-SPC") 'jedi:complete)
  )

(use-package ruby-mode
  :mode (("\\.rb\\'" . ruby-mode)
         ("\\.rake\\'" . ruby-mode)
         ("Rakefile\\'" . ruby-mode))
  :interpreter "ruby"
  :config
  (progn
    (my/setup-run-code ruby-mode-map "ruby")

    (require 'ruby-mode-indent-fix)
    (setq ruby-indent-level 4)
    (setq ruby-deep-indent-paren nil)

    (setq rsense-home "/usr/local/Cellar/rsense/0.3/libexec")
    (when (file-directory-p rsense-home)
      (add-to-list 'load-path (concat rsense-home "/etc"))
      (require 'rsense)
      (add-hook 'ruby-mode-hook 'my/ac-ruby-setup))

    (use-package rbenv :ensure t)
    (use-package inf-ruby :ensure t
      :config
      (evil-set-initial-state 'inf-ruby-mode 'insert)
      :init
      (add-to-list 'ac-modes 'inf-ruby-mode))
    (use-package ac-inf-ruby :ensure t
      :init
      (add-hook 'inf-ruby-mode-hook 'ac-inf-ruby-enable))))

;;
(provide 'mod-dev)
