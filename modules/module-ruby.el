;;; module-ruby.el

;; Silence the byte-compiler
(eval-when-compile (require 'defuns-quickrun))

(use-package ruby-mode
  :mode ("\\.rb$" "\\.rake$" "\\.gemspec$" "\\.?pryrc$"
         "/\\(Gem\\|Cap\\|Vagrant\\|Rake\\)file$")
  :interpreter "ruby"
  :init
  (add-hook! ruby-mode 'flycheck-mode)
  (define-docset!      ruby-mode "rb,ruby,rubygem")
  (define-builder!     ruby-mode "rake %s" "Rakefile")
  (define-env-command! ruby-mode "ruby --version | cut -d' ' -f2")
  :config
  ;; Formatting
  (setq ruby-deep-indent-paren t)

  (add-hook! ruby-mode
    (electric-indent-local-mode +1)
    (setq narf-electric-indent-words '("else" "end" "elseif")))

  ;; Don't interfere with my custom RET behavior
  (define-key ruby-mode-map [?\n] nil)

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
            (refactor-extract-to-let          "extract to let"          t)
            (refactor-convert-post-conditional "convert post conditional" t)))))

;; Rakefiles ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-minor-mode rake-mode
  "Buffer local minor mode for rake files"
  :lighter " Rake" :keymap (make-sparse-keymap)
  (add-yas-minor-mode! 'rake-mode))
(associate! rake-mode :match "\\(/Rakefile\\|\\.rake\\)$")
(associate! rake-mode :files ("Rakefile"))

;; Vagrantfiles ;;;;;;;;;;;;;;;;;;;;;;;;
(define-minor-mode vagrant-mode
  "Buffer local minor mode for vagrant files"
  :lighter " Va" :keymap (make-sparse-keymap)
  (add-yas-minor-mode! 'vagrant-mode))
(associate! vagrant-mode :files ("Vagrantfile"))

(use-package rspec-mode
  :defer t
  :mode ("/\\.rspec$" . text-mode)
  :init
  (associate! rspec-mode :match "/\\.rspec$")
  (associate! rspec-mode :in (ruby-mode yaml-mode) :files ("spec/"))

  (defvar rspec-mode-verifiable-map (make-sparse-keymap))
  (defvar evilmi-ruby-match-tags
    '((("unless" "if") ("elsif" "else") "end")
      ("begin" ("rescue" "ensure") "end")
      ("case" ("when" "else") "end")
      (("class" "def" "while" "do" "module" "for" "until") () "end")
      ;; Rake
      (("task" "namespace") () "end")))
  :config
  (map! :map rspec-mode-map
        (:localleader
         :n "tr" 'rspec-rerun
         :n "ta" 'rspec-verify-all
         :n "ts" 'rspec-verify-single
         :n "tv" 'rspec-verify)))

(use-package inf-ruby
  :commands (inf-ruby inf-ruby-console-auto)
  :init
  (define-repl! ruby-mode inf-ruby)
  :config
  (evil-set-initial-state 'inf-ruby-mode 'emacs)
  (require 'company-inf-ruby)
  (define-company-backend! inf-ruby-mode (inf-ruby)))

(use-package robe
  :commands (robe-mode robe-start ruby-load-file)
  :init
  (add-hook! ruby-mode
    (narf|ruby-load-file)
    (add-hook 'after-save-hook 'narf|ruby-load-file nil t))
  :config
  (require 'company-robe)
  (define-company-backend! ruby-mode (robe)))

(provide 'module-ruby)
;;; module-ruby.el ends here
