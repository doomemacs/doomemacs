;;; module-ruby.el

(use-package ruby-mode
  :mode ("\\.rb$" "\\.rake$" "\\.gemspec$" "\\.?pryrc$"
         "/\\(Gem\\|Cap\\|Vagrant\\|Rake\\)file$")
  :interpreter "ruby"
  :init
  (def-builder! ruby-mode "rake %s" "Rakefile")
  (def-company-backend! ruby-mode (dabbrev-code))
  (def-docset! ruby-mode "rb,ruby,rubygem")
  (def-env-command! ruby-mode "ruby --version | cut -d' ' -f2")
  (def-repl! ruby-mode inf-ruby)
  (def-electric! ruby-mode :words ("else" "end" "elseif"))
  (add-hook! 'ruby-mode-hook '(flycheck-mode yard-mode))

  :config
  (setq ruby-deep-indent-paren t) ; Formatting

  ;; Don't interfere with my custom RET behavior
  (define-key ruby-mode-map [?\n] nil))

(use-package ruby-refactor
  :after ruby-mode
  :init (add-hook 'ruby-mode-hook 'emr-initialize)
  :config
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
          (refactor-convert-post-conditional "convert post conditional" t))))

;; Highlight doc comments
(use-package yard-mode :commands yard-mode)

(use-package rspec-mode
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
  :init (def-company-backend! inf-ruby-mode (inf-ruby)))

(use-package company-inf-ruby :after inf-ruby)

;;
(def-project-type! rake "rake"
  :files ("Rakefile"))

(provide 'module-ruby)
;;; module-ruby.el ends here
