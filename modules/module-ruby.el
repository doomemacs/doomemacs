;;; module-ruby.el

;; Silence the byte-compiler
(eval-when-compile (require 'defuns-quickrun))

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
  (add-hook! enh-ruby-mode 'narf|enable-tab-width-2)
  (build-for! enh-ruby-mode "rake %s" "Rakefile")
  :config
	;;; Formatting
  (setq ruby-indent-level      2
        ruby-deep-indent-paren t
        enh-ruby-check-syntax  nil)

  (associate! text-mode :match "/\\.rspec$")

  ;; Don't interfere with my custom RET behavior
  (define-key enh-ruby-mode-map [?\n] nil)

  (use-package ruby-refactor
    :init (add-hook! enh-ruby-mode 'emr-initialize)
    :config
    (after! emr
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
    (add-yas-minor-mode! 'rake-mode))
  (associate! rake-mode :match "/\\(Rakefile\\|\\.rake\\)$")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Vagrantfiles ;;;;;;;;;;;;;;;;;;;;;;;;
  (define-minor-mode vagrant-mode
    "Buffer local minor mode for vagrant files"
    :lighter " Va" :keymap (make-sparse-keymap)
    (add-yas-minor-mode! 'vagrant-mode))
  (associate! vagrant-mode :match "/Vagrantfile$")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Rspec files ;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package rspec-mode
    :defer t
    :init
    (associate! rspec-mode :match "\\(/spec_helper\\|_spec\\)\\.rb$")
    (associate! rspec-mode :match "/\\.rspec$")

    (defvar rspec-mode-verifiable-map (make-sparse-keymap))
    (defvar evilmi-ruby-match-tags
      '((("unless" "if") ("elsif" "else") "end")
        ("begin" ("rescue" "ensure") "end")
        ("case" ("when" "else") "end")
        (("class" "def" "while" "do" "module" "for" "until") () "end")
        ;; Rake
        (("task" "namespace") () "end")))
    :config
    (bind! (:prefix ","
             :n "tr" 'rspec-rerun
             :n "ta" 'rspec-verify-all
             :n "ts" 'rspec-verify-single
             :n "tv" 'rspec-verify)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (use-package inf-ruby
    :commands (inf-ruby inf-ruby-console-auto)
    :config
    (evil-set-initial-state 'inf-enh-ruby-mode 'insert)
    (after! company
      (require 'company-inf-ruby)
      (add-company-backend! inf-enh-ruby-mode (inf-ruby))))

  (use-package robe
    :functions (robe-mode robe-start ruby-load-file)
    :config
    (add-hook! after-save 'narf|ruby-load-file)
    (add-hook! enh-ruby-mode 'narf|enable-robe-maybe)

    (after! company
      (require 'company-robe)
      (add-company-backend! enh-ruby-mode (robe)))))

(provide 'module-ruby)
;;; module-ruby.el ends here
