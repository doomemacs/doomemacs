(provide 'init-ruby)

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
  :init
  (progn
    (associate-minor-mode "\\(/spec_helper\\|_spec\\)\\.rb$" 'rspec-mode)
    (associate-minor-mode "/\\.rspec$" 'rspec-mode)
    (associate-minor-mode "/\\.rake$" 'rake-mode)
    (associate-mode "/\\.rspec$" 'text-mode))
  :config
  (progn
	;;; Formatting
    (setq ruby-indent-level 2)
    (setq ruby-deep-indent-paren t)

	(add-hook 'ruby-mode-hook 'enable-tab-width-2)

    (define-key ruby-mode-map [?\n] nil)

    (after "emr"
      (use-package ruby-refactor)
      (emr-declare-command 'ruby-refactor-extract-to-method
        :title "extract method"
        :modes 'ruby-mode
        :predicate (lambda () (use-region-p)))

      (emr-declare-command 'ruby-refactor-extract-local-variable
        :title "extract local variable"
        :modes 'ruby-mode
        :predicate (lambda () (use-region-p)))

      (emr-declare-command 'ruby-refactor-extract-constant
        :title "extract constant"
        :modes 'ruby-mode
        :predicate (lambda () (use-region-p)))

      (emr-declare-command 'ruby-refactor-add-parameter
        :title "add parameter"
        :modes 'ruby-mode)

      (emr-declare-command 'ruby-refactor-extract-to-let
        :title "extract to let"
        :modes 'ruby-mode
        :predicate (lambda () (use-region-p))))

    (define-minor-mode rake-mode
      "Buffer local minor mode for rake files"
      :lighter " Rake")

    (use-package inf-ruby
      :commands (inf-ruby inf-ruby-console-auto)
      :config
      (progn
        (evil-set-initial-state 'inf-ruby-mode 'insert)
        (push '(inf-ruby-mode :position bottom :stick t) popwin:special-display-config)

        (after "company"
          (use-package company-inf-ruby
            :config (company--backend-on 'inf-ruby-mode-hook 'company-inf-ruby)))))

    (use-package rspec-mode
      :defer t
      :pre-load
      (progn
        (defvar rspec-mode-verifiable-map (make-sparse-keymap))
        (defvar evilmi-ruby-match-tags
          '((("unless" "if") ("elsif" "else") "end")
            ("begin" ("rescue" "ensure") "end")
            ("case" ("when" "else") "end")
            (("class" "def" "while" "do" "module" "for" "until") () "end")
            ;; Rake
            (("task" "namespace") () "end"))))
      :config
      (bind 'normal
            ",tr" 'rspec-rerun
            ",ta" 'rspec-verify-all
            ",ts" 'rspec-verify-single
            ",tv" 'rspec-verify))

    (use-package robe
      :config
      (progn
        (after "company"
          (use-package company-robe
            :config (company--backend-on 'ruby-mode-hook 'company-robe)))

        (add-hook! 'ruby-mode-hook
          (robe-mode 1)
          ;; (after "auto-complete" (ac-robe-setup))
          (unless robe-running (robe-start 1))
          (my--ruby-load-file buffer-file-name))

        (defun my--ruby-load-file (&optional file)
          (let ((file (or file buffer-file-name)))
            (when (and (eq major-mode 'ruby-mode)
                       (bound-and-true-p robe-running)
                       (file-exists-p buffer-file-name))
              (ruby-load-file file))))
        (add-hook 'after-save-hook 'my--ruby-load-file)))))
