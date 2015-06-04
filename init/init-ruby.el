(use-package enh-ruby-mode
  :mode (("\\.rb$"        . enh-ruby-mode)
         ("\\.ru$"        . enh-ruby-mode)
         ("\\.rake$"      . enh-ruby-mode)
         ("\\.gemspec$"   . enh-ruby-mode)
         ("\\.?pryrc$"    . enh-ruby-mode)
         ("/Gemfile$"     . enh-ruby-mode)
         ("/Capfile$"     . enh-ruby-mode)
         ("/Vagrantfile$" . enh-ruby-mode)
         ("/Rakefile$"    . enh-ruby-mode))
  :interpreter "ruby"
  :init
  (progn
	(add-hook 'enh-ruby-mode-hook 'narf|enable-tab-width-2)
    (add-hook! 'enh-ruby-mode-hook (set-build-command "rake %s" "Rakefile")))
  :config
  (progn
	;;; Formatting
    (setq ruby-indent-level      2
          ruby-deep-indent-paren t
          enh-ruby-check-syntax  nil)

    (associate-mode "/\\.rspec$" 'text-mode)

    ;; Don't interfere with my custom RET behavior
    (define-key enh-ruby-mode-map [?\n] nil)

    (use-package ruby-refactor
      :init
      (add-hook 'enh-ruby-mode-hook 'emr-initialize)
      :config
      (after "emr"
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
          :predicate (lambda () (use-region-p)))))


    ;; Rakefiles ;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define-minor-mode rake-mode
      "Buffer local minor mode for rake files"
      :lighter " Rake" :keymap (make-sparse-keymap)
      (narf/init-yas-mode 'rake-mode))
    (associate-minor-mode "/\\(Rakefile\\|\\.rake\\)$" 'rake-mode)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; Vagrantfiles ;;;;;;;;;;;;;;;;;;;;;;;;
    (define-minor-mode vagrant-mode
      "Buffer local minor mode for vagrant files"
      :lighter " Va" :keymap (make-sparse-keymap)
      (narf/yas-init-mode 'vagrant-mode))
    (associate-minor-mode "/Vagrantfile$" 'vagrant-mode)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; Rspec files ;;;;;;;;;;;;;;;;;;;;;;;;;
    (use-package rspec-mode
      :defer t
      :init
      (progn
        (associate-minor-mode "\\(/spec_helper\\|_spec\\)\\.rb$" 'rspec-mode)
        (associate-minor-mode "/\\.rspec$"                       'rspec-mode)

        (defvar rspec-mode-verifiable-map (make-sparse-keymap))
        (defvar evilmi-ruby-match-tags
          '((("unless" "if") ("elsif" "else") "end")
            ("begin" ("rescue" "ensure") "end")
            ("case" ("when" "else") "end")
            (("class" "def" "while" "do" "module" "for" "until") () "end")
            ;; Rake
            (("task" "namespace") () "end"))))
      :config
      (bind :normal
            ",tr" 'rspec-rerun
            ",ta" 'rspec-verify-all
            ",ts" 'rspec-verify-single
            ",tv" 'rspec-verify))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (use-package inf-ruby
      :commands (inf-ruby inf-ruby-console-auto)
      :config
      (progn
        (evil-set-initial-state 'inf-enh-ruby-mode 'insert)
        (after "company"
          (use-package company-inf-ruby
            :config (narf/add-company-backend inf-enh-ruby-mode (company-inf-ruby))))))

    (use-package robe
      :functions (robe-mode robe-start ruby-load-file)
      :config
      (progn
        (after "company"
          (use-package company-robe
            :config (narf/add-company-backend enh-ruby-mode (company-robe))))

        (defun narf|enable-robe-maybe ()
          (let ((file (buffer-file-name)))
            ;; Don't run in gemfiles, capfiles or vagrantfiles
            (unless (or (string-equal (f-filename file) "Gemfile")
                        (string-equal (f-filename file) "Capfile")
                        (string-equal (f-filename file) "Vagrantfile")
                        (f-ext? file "org")) ;; or org-mode
              (robe-mode 1)
              (narf|ruby-load-file file))))
        (add-hook 'enh-ruby-mode-hook 'narf|enable-robe-maybe)

        (defun narf|ruby-load-file (&optional file)
          (let ((file (or file buffer-file-name)))
            (when (and (eq major-mode 'enh-ruby-mode)
                       (featurep 'robe)
                       (not (string= (f-base file) "Gemfile"))
                       (file-exists-p buffer-file-name))
              (unless robe-running (robe-start 1))
              (when robe-running (ruby-load-file file)))))
        (add-hook 'after-save-hook 'narf|ruby-load-file)))))


(provide 'init-ruby)
;;; init-ruby.el ends here
