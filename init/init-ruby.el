(provide 'init-ruby)

;;
(use-package ruby-mode
  :mode (("\\.rb$" . ruby-mode)
         ("\\.ru$" . ruby-mode)
         ("\\.rake$" . ruby-mode)
         ("\\.gemspec$" . ruby-mode)
         ("\\.?pryrc$" . ruby-mode)
         ("Gemfile$" . ruby-mode)
         ("Capfile$" . ruby-mode)
         ("Vagrantfile$" . ruby-mode)
         ("Rakefile$" . ruby-mode))
  :interpreter "ruby"
  :config
  (progn
    (define-minor-mode rake-mode
      "Buffer local minor mode for rake files"
      :lighter " Rake")

    (use-package inf-ruby
      :config
      (progn
        (evil-set-initial-state 'inf-ruby-mode 'insert)
        ;; (use-package ac-inf-ruby)
        ;; (add-hook 'inf-ruby-mode-hook 'ac-inf-ruby-enable)

        (push '(inf-ruby-mode :position bottom :stick t) popwin:special-display-config)))

    (use-package rspec-mode
      :defer t
      :pre-load
      (defvar evilmi-ruby-match-tags
        '((("unless" "if") ("elsif" "else") "end")
          ("begin" ("rescue" "ensure") "end")
          ("case" ("when" "else") "end")
          (("class" "def" "while" "do" "module" "for" "until") () "end")
          ;; Rake
          (("task" "namespace") () "end")
          ))
      :init
      (progn (associate-minor-mode "\\(/spec_helper\\|_spec\\)\\.rb$" rspec-mode)
             (associate-minor-mode "/\\.rspec$" rspec-mode)
             (associate-minor-mode "/\\.rake$" rake-mode)
             (associate-mode "/\\.rspec$" text-mode))
      :config
      (progn (bind 'normal rspec-mode-verifiable-keymap
                   ",tr" 'rspec-rerun
                   ",ta" 'rspec-verify-all
                   ",ts" 'rspec-verify-single
                   ",tv" 'rspec-verify)
             (bind 'normal rspec-dired-mode-keymap
                   ",tv" 'rspec-dired-verify
                   ",ts" 'rspec-dired-verify-single
                   ",ta" 'rspec-verify-all
                   ",tr" 'rspec-rerun)))

    (use-package robe
      :config
      (progn
        (add-hook! 'after-save-hook
                   (when (and (eq major-mode 'ruby-mode)
                              (bound-and-true-p robe-running))
                     (ruby-load-file (buffer-file-name))))
        (add-hook! 'ruby-mode-hook
                   (robe-mode 1)
                   (ac-robe-setup)
                   (unless robe-running (robe-start 1))
                   (ruby-load-file (buffer-file-name)))))

    (add-hook! 'ruby-mode-hook
               (setq my-switch-to-repl-func 'ruby-switch-to-inf
                     my-send-region-to-repl-func 'ruby-send-region
                     my-run-code-interpreter "ruby"))

	;;; Formatting
    (setq ruby-indent-level 2)
    (setq ruby-deep-indent-paren t)
	(add-hook 'ruby-mode-hook 'enable-tab-width-2)))
