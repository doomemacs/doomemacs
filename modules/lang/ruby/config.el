;;; lang/ruby/config.el -*- lexical-binding: t; -*-

(def-package! ruby-mode
  :mode ("\\.rb$" "\\.rake$" "\\.gemspec$" "\\.?pryrc$"
         "/\\(Gem\\|Cap\\|Vagrant\\|Rake\\|Pod\\|Puppet\\|Berks\\)file$")
  :interpreter "ruby"
  :init
  (add-hook 'ruby-mode-hook #'flycheck-mode)
  :config
  (set! :company-backend 'ruby-mode '(company-dabbrev-code))
  (set! :electric 'ruby-mode :words '("else" "end" "elseif"))
  (setq ruby-deep-indent-paren t)
  ;; Don't interfere with my custom RET behavior
  (define-key ruby-mode-map [?\n] nil)

  (map! :map ruby-mode-map
        :localleader
        :prefix "r"
        :nv "b"  #'ruby-toggle-block
        :nv "ec" #'ruby-refactor-extract-constant
        :nv "el" #'ruby-refactor-extract-to-let
        :nv "em" #'ruby-refactor-extract-to-method
        :nv "ev" #'ruby-refactor-extract-local-variable
        :nv "ad" #'ruby-refactor-add-parameter
        :nv "cc" #'ruby-refactor-convert-post-conditional))


(def-package! ruby-refactor
  :commands
  (ruby-refactor-extract-to-method ruby-refactor-extract-local-variable
   ruby-refactor-extract-constant ruby-refactor-add-parameter
   ruby-refactor-extract-to-let ruby-refactor-convert-post-conditional))


;; Highlight doc comments
(def-package! yard-mode
  :commands yard-mode
  :init (add-hook 'ruby-mode-hook #'yard-mode))


(def-package! rspec-mode
  :mode ("/\\.rspec$" . text-mode)
  :init
  (associate! rspec-mode :match "/\\.rspec$")
  (associate! rspec-mode :modes (ruby-mode yaml-mode) :files ("/spec/"))
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
        :localleader
        :prefix "t"
        :n "r" #'rspec-rerun
        :n "a" #'rspec-verify-all
        :n "s" #'rspec-verify-single
        :n "v" #'rspec-verify))


(def-package! inf-ruby
  :commands (inf-ruby inf-ruby-console-auto)
  :init (set! :repl 'ruby-mode 'inf-ruby))


(def-package! company-inf-ruby
  :when (featurep! :completion company)
  :after inf-ruby
  :config (set! :company-backend 'inf-ruby-mode '(company-inf-ruby)))


;;
;; TODO Frameworks
;;

;; (def-project-mode! +ruby-rake-mode
;;   :files "Rakefile"
;;   :init
;;   (set! :build 'rake '+ruby-rake-mode '+ruby/rake
;;     :when (lambda () (doom-project-has! "Rakefile"))))
