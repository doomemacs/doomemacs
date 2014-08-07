(mapc 'my/install-package
      '(yaml-mode
        jedi
        python-mode
        inf-ruby
        ac-inf-ruby
        rbenv
        json-mode
        ))

(use-package yaml-mode :mode "\\.yaml\\'")

(use-package json-mode
  :mode (("\\.json\\'" . json-mode)
         ("\\.jshintrc\\'" . json-mode)))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (progn
    (use-package jedi
      :init
      (if (not (file-directory-p "~/.emacs.d/.python-environments/default/"))
          (jedi:install-server)))

    ;; Let autopair work with triple-quotes
    (setq autopair-handle-action-fns
          (list #'autopair-default-handle-action
                #'autopair-python-triple-quote-action))

    ;; Don't remap backspace. Leave it to autopair, please.
    (define-key python-mode-map [backspace] nil)
    (nmap python-mode-map (kbd "s-r")
          (lambda()
            (interactive)
            (shell-command-on-region (point-min) (point-max) "python")))
    (vmap python-mode-map (kbd "s-r")
          (lambda()
            (interactive)
            (shell-command-on-region (region-beginning) (region-end) "python")))
    )

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
    (require 'ruby-mode-indent-fix)
    (evil-set-initial-state 'inf-ruby-mode 'insert)

    (setq ruby-indent-level 4)
    (setq ruby-deep-indent-paren nil)

    (use-package rbenv)
    (use-package inf-ruby
      :init
      (add-to-list 'ac-modes 'inf-ruby-mode))
    (use-package ac-inf-ruby
      :init
      (add-hook 'inf-ruby-mode-hook 'ac-inf-ruby-enable)))
  )


;;
(provide 'mod-dev)
