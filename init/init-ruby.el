(use-package enh-ruby-mode
  :mode (("\\.rb$" . enh-ruby-mode)
         ("\\.ru$" . enh-ruby-mode)
         ("\\.rake$" . enh-ruby-mode)
         ("\\.gemspec$" . enh-ruby-mode)
         ("\\.?pryrc$" . enh-ruby-mode)
         ("Gemfile$" . enh-ruby-mode)
         ("Capfile$" . enh-ruby-mode)
         ("Vagrantfile$" . enh-ruby-mode)
         ("Rakefile$" . enh-ruby-mode))
  :interpreter "ruby"
  :init
  (progn
    (associate-minor-mode "\\(/spec_helper\\|_spec\\)\\.rb$" 'rspec-mode)
    (associate-minor-mode "/\\.rspec$" 'rspec-mode)
    (associate-minor-mode "/\\.rake$" 'rake-mode)
    (associate-mode "/\\.rspec$" 'text-mode))
  :config
  (progn
    (message "Ruby loaded!")
	;;; Formatting
    (setq ruby-indent-level 2)
    (setq ruby-deep-indent-paren t)
    (setq enh-ruby-check-syntax nil)

	(add-hook 'enh-ruby-mode-hook 'enable-tab-width-2)

    (define-key enh-ruby-mode-map [?\n] nil)

    (after "emr"
      (use-package ruby-refactor)
      (emr-declare-command 'ruby-refactor-extract-to-method
        :title "extract method"
        :modes 'enh-ruby-mode
        :predicate (lambda () (use-region-p)))

      (emr-declare-command 'ruby-refactor-extract-local-variable
        :title "extract local variable"
        :modes 'enh-ruby-mode
        :predicate (lambda () (use-region-p)))

      (emr-declare-command 'ruby-refactor-extract-constant
        :title "extract constant"
        :modes 'enh-ruby-mode
        :predicate (lambda () (use-region-p)))

      (emr-declare-command 'ruby-refactor-add-parameter
        :title "add parameter"
        :modes 'enh-ruby-mode)

      (emr-declare-command 'ruby-refactor-extract-to-let
        :title "extract to let"
        :modes 'enh-ruby-mode
        :predicate (lambda () (use-region-p))))

    (define-minor-mode rake-mode
      "Buffer local minor mode for rake files"
      :lighter " Rake")

    (use-package inf-ruby
      :commands (inf-ruby inf-ruby-console-auto)
      :config
      (progn
        (evil-set-initial-state 'inf-enh-ruby-mode 'insert)
        (push '(inf-enh-ruby-mode :position bottom :stick t) popwin:special-display-config)

        (after "company"
          (use-package company-inf-ruby
            :config (company--backend-on 'inf-enh-ruby-mode-hook 'company-inf-ruby)))))

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
            :config (company--backend-on 'enh-ruby-mode-hook 'company-robe)))

        (add-hook! 'enh-ruby-mode-hook
          (unless (f-ext? (buffer-file-name) "org") ;; in case of org-mode
            (robe-mode 1)
            ;; (after "auto-complete" (ac-robe-setup))
            (my--ruby-load-file buffer-file-name)))

        (defun my--ruby-load-file (&optional file)
          (let ((file (or file buffer-file-name)))
            (when (and (eq major-mode 'enh-ruby-mode)
                       (featurep 'robe)
                       (not (string= (f-base file) "Gemfile"))
                       (file-exists-p buffer-file-name))
              (unless robe-running (robe-start 1))
              (when robe-running (ruby-load-file file)))))
        (add-hook 'after-save-hook 'my--ruby-load-file)))))


(provide 'init-ruby)
;;; init-ruby.el ends here
