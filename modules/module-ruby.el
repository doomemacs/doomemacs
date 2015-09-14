;;; module-ruby.el

;; Silence the byte-compiler
(eval-when-compile (require 'defuns-quickrun))

(use-package ruby-mode
  :mode ("\\.r[bu]$" "\\.rake$" "\\.gemspec$" "\\.?pryrc$"
         "/\\(Gem\\|Cap\\|Vagrant\\|Rake\\)file$")
  :interpreter "ruby"
  :init
  (add-hook! ruby-mode 'narf|enable-tab-width-2)
  (build-for! ruby-mode "rake %s" "Rakefile")
  :config
  ;; Formatting
  (setq ruby-indent-level      2
        ruby-deep-indent-paren t)

  ;; Don't interfere with my custom RET behavior
  (define-key ruby-mode-map [?\n] nil)

  ;; Rakefiles ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define-minor-mode rake-mode
    "Buffer local minor mode for rake files"
    :lighter " Rake" :keymap (make-sparse-keymap)
    (add-yas-minor-mode! 'rake-mode))
  (associate! rake-mode :match "\\(/Rakefile\\|\\.rake\\)$")

  ;; Vagrantfiles ;;;;;;;;;;;;;;;;;;;;;;;;
  (define-minor-mode vagrant-mode
    "Buffer local minor mode for vagrant files"
    :lighter " Va" :keymap (make-sparse-keymap)
    (add-yas-minor-mode! 'vagrant-mode))
  (associate! vagrant-mode :match "/Vagrantfile$"))

(use-package rspec-mode
  :defer t
  :mode ("/\\.rspec$" . text-mode)
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

(use-package inf-ruby
  :commands (inf-ruby inf-ruby-console-auto)
  :config
  (evil-set-initial-state 'inf-ruby-mode 'insert)
  (after! company
    (require 'company-inf-ruby)
    (add-company-backend! inf-ruby-mode (inf-ruby))))

(use-package robe
  :commands (robe-mode robe-start ruby-load-file)
  :init
  (add-hook! after-save 'narf|ruby-load-file)
  (add-hook! ruby-mode 'narf|enable-robe-maybe)
  :config
  (after! company
    (require 'company-robe)
    (add-company-backend! ruby-mode (robe))))

(use-package ruby-refactor
  :after emr
  :init (add-hook! ruby-mode 'emr-initialize)
  :config
  (emr-declare-command 'ruby-toggle-block
    :title "toggle block"
    :modes 'enh-ruby-mode
    :predicate (lambda () (not (use-region-p))))
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
    :modes 'enh-ruby-mode
    :predicate (lambda () (not (use-region-p))))
  (emr-declare-command 'ruby-refactor-extract-to-let
    :title "extract to let"
    :modes 'enh-ruby-mode
    :predicate (lambda () (use-region-p))))

(provide 'module-ruby)
;;; module-ruby.el ends here
