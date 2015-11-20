;;; module-ruby.el

;; Silence the byte-compiler
(eval-when-compile (require 'defuns-quickrun))

(use-package ruby-mode
  :mode ("\\.rb$" "\\.rake$" "\\.gemspec$" "\\.?pryrc$"
         "/\\(Gem\\|Cap\\|Vagrant\\|Rake\\)file$")
  :interpreter "ruby"
  :init
  (add-hook! ruby-mode '(narf|enable-tab-width-2 flycheck-mode))
  :config
  (define-builder! ruby-mode "rake %s" "Rakefile")
  (define-env-command! ruby-mode "ruby --version | cut -d' ' -f2")
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
  (bind! (:map rspec-mode-map
           :n ",tr" 'rspec-rerun
           :n ",ta" 'rspec-verify-all
           :n ",ts" 'rspec-verify-single
           :n ",tv" 'rspec-verify)))

(use-package inf-ruby
  :commands (inf-ruby inf-ruby-console-auto)
  :config
  (evil-set-initial-state 'inf-ruby-mode 'insert)
  (after! company
    (require 'company-inf-ruby)
    (define-company-backend! inf-ruby-mode (inf-ruby))))

(use-package robe
  :diminish "R"
  :commands (robe-mode robe-start ruby-load-file)
  :init
  (add-hook! ruby-mode
    (narf|ruby-load-file)
    (add-hook 'after-save-hook 'narf|ruby-load-file nil t))
  :config
  (after! company
    (require 'company-robe)
    (define-company-backend! ruby-mode (robe))))

(use-package ruby-refactor
  :init (add-hook! ruby-mode 'emr-initialize)
  :config
  (require 'emr)
  (mapc (lambda (x)
          (let ((command-name (car x))
                (title (cadr x))
                (region-p (caddr x))
                predicate)
            (setq predicate (cond ((eq region-p 'both) nil)
                                  (t (if region-p
                                         (lambda () (use-region-p))
                                       (lambda () (not (use-region-p)))))))
            (emr-declare-command (intern (format "ruby-%s" (symbol-name command-name)))
              :title title :modes 'ruby-mode :predicate predicate)))
        '((toggle-block                     "toggle block"            nil)
          (refactor-extract-to-method       "extract method"          t)
          (refactor-extract-local-variable  "extract local variable"  t)
          (refactor-extract-constant        "extract constant"        t)
          (refactor-add-parameter           "add parameter"           nil)
          (refactor-extract-to-let          "extract to let"          t))))

(provide 'module-ruby)
;;; module-ruby.el ends here
