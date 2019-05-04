;;; lang/ruby/config.el -*- lexical-binding: t; -*-

;;
;; Packages

(def-package! enh-ruby-mode
  :mode ("\\.\\(?:pry\\|irb\\)rc\\'" . +ruby|init)
  :mode ("\\.\\(?:rb\\|rake\\|rabl\\|ru\\|builder\\|gemspec\\|jbuilder\\|thor\\)\\'" .  +ruby|init)
  :mode ("/\\(?:Berks\\|Cap\\|Gem\\|Guard\\|Pod\\|Puppet\\|Rake\\|Thor\\|Vagrant\\)file\\'" .  +ruby|init)
  :preface
  (after! ruby-mode
    (require 'enh-ruby-mode))
  (defun +ruby|init ()
    "Enable `enh-ruby-mode' if ruby is available, otherwise `ruby-mode'."
    (if (executable-find "ruby")
        (enh-ruby-mode)
      (ruby-mode)))
  :config
  (set-electric! '(ruby-mode enh-ruby-mode) :words '("else" "end" "elsif"))
  (set-repl-handler! '(ruby-mode enh-ruby-mode) #'inf-ruby)

  (when (featurep! +lsp)
    (add-hook 'enh-ruby-mode-hook #'lsp!))

  (after! company-dabbrev-code
    (add-to-list 'company-dabbrev-code-modes 'enh-ruby-mode nil #'eq)
    (add-to-list 'company-dabbrev-code-modes 'ruby-mode nil #'eq))

  ;; so class and module pairs work
  (setq-hook! (ruby-mode enh-ruby-mode) sp-max-pair-length 6))


(def-package! robe
  :defer t
  :init
  (defun +ruby|init-robe-mode-maybe ()
    "Start `robe-mode' if `lsp-mode' isn't active."
    (unless (bound-and-true-p lsp-mode)
      (robe-mode +1)))
  (add-hook 'enh-ruby-mode-hook #'+ruby|init-robe-mode-maybe)
  :config
  (set-repl-handler! 'enh-ruby-mode #'robe-start)
  (set-company-backend! 'enh-ruby-mode 'company-robe)
  (set-lookup-handlers! 'enh-ruby-mode
    :definition #'robe-jump
    :documentation #'robe-doc)
  (map! :localleader
        :map robe-mode-map
        "'"  #'robe-start
        ;; robe mode specific
        "h"  #'robe-doc
        "rr" #'robe-rails-refresh
        ;; inf-enh-ruby-mode
        :prefix "s"
        "f"  #'ruby-send-definition
        "F"  #'ruby-send-definition-and-go
        "r"  #'ruby-send-region
        "R"  #'ruby-send-region-and-go
        "i"  #'ruby-switch-to-inf))


;; NOTE Must be loaded before `robe-mode'
(def-package! yard-mode
  :hook (ruby-mode enh-ruby-mode))


(def-package! rubocop
  :hook (enh-ruby-mode . rubocop-mode)
  :config
  (map! :localleader
        :map rubocop-mode-map
        "f" #'rubocop-check-current-file
        "F" #'rubocop-autocorrect-current-file
        "p" #'rubocop-check-project
        "P" #'rubocop-autocorrect-project))


;;
;; Package & Ruby version management

(def-package! rake
  :defer t
  :init
  (setq rake-cache-file (concat doom-cache-dir "rake.cache"))
  (map! :after enh-ruby-mode
        :localleader
        :map enh-ruby-mode-map
        :prefix "k"
        "k" #'rake
        "r" #'rake-rerun
        "R" #'rake-regenerate-cache
        "f" #'rake-find-task))

(def-package! bundler
  :defer t
  :init
  (map! :after enh-ruby-mode
        :localleader
        :map enh-ruby-mode-map
        :prefix "b"
        "c" #'bundle-check
        "C" #'bundle-console
        "i" #'bundle-install
        "u" #'bundle-update
        "e" #'bundle-exec
        "o" #'bundle-open))

(after! rbenv
  (setq rspec-use-rvm nil)
  (add-to-list 'exec-path (expand-file-name "shims" rbenv-installation-dir)))


;;
;; Testing frameworks

(def-package! rspec-mode
  :mode ("/\\.rspec\\'" . text-mode)
  :init
  (defvar evilmi-ruby-match-tags
    '((("unless" "if") ("elsif" "else") "end")
      ("begin" ("rescue" "ensure") "end")
      ("case" ("when" "else") "end")
      (("class" "def" "while" "do" "module" "for" "until") () "end")
      ;; Rake
      (("task" "namespace") () "end")))

  (when (featurep! :editor evil)
    (add-hook 'rspec-mode-hook #'evil-normalize-keymaps))
  :config
  (setq rspec-use-rvm (executable-find "rvm"))
  (map! :localleader
        :prefix "t"
        :map (rspec-verifiable-mode-map rspec-dired-mode-map rspec-mode-map)
        "a" #'rspec-verify-all
        "r" #'rspec-rerun
        :map (rspec-verifiable-mode-map rspec-mode-map)
        "v" #'rspec-verify
        "c" #'rspec-verify-continue
        "l" #'rspec-run-last-failed
        "T" #'rspec-toggle-spec-and-target
        "t" #'rspec-toggle-spec-and-target-find-example
        :map rspec-verifiable-mode-map
        "f" #'rspec-verify-method
        "m" #'rspec-verify-matching
        :map rspec-mode-map
        "s" #'rspec-verify-single
        "e" #'rspec-toggle-example-pendingness
        :map rspec-dired-mode-map
        "v" #'rspec-dired-verify
        "s" #'rspec-dired-verify-single))


(def-package! minitest
  :defer t
  :config
  (when (featurep! :editor evil)
    (add-hook 'minitest-mode-hook #'evil-normalize-keymaps))
  (map! :localleader
        :map minitest-mode-map
        :prefix "t"
        "r" #'minitest-rerun
        "a" #'minitest-verify-all
        "s" #'minitest-verify-single
        "v" #'minitest-verify))
