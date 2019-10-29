;;; lang/ruby/config.el -*- lexical-binding: t; -*-

(after! projectile
  (add-to-list 'projectile-project-root-files "Gemfile"))


;;
;;; Packages

(use-package! enh-ruby-mode
  :mode ("\\.\\(?:pry\\|irb\\)rc\\'" . +ruby-init-h)
  :mode ("\\.\\(?:rb\\|rake\\|rabl\\|ru\\|builder\\|gemspec\\|jbuilder\\|thor\\)\\'" .  +ruby-init-h)
  :mode ("/\\(?:Berks\\|Cap\\|Gem\\|Guard\\|Pod\\|Puppet\\|Rake\\|Thor\\|Vagrant\\)file\\'" .  +ruby-init-h)
  :preface
  (after! ruby-mode
    (require 'enh-ruby-mode))
  (defun +ruby-init-h ()
    "Enable `enh-ruby-mode' if ruby is available, otherwise `ruby-mode'."
    (if (executable-find "ruby")
        (enh-ruby-mode)
      (ruby-mode)))
  :config
  (set-electric! '(ruby-mode enh-ruby-mode) :words '("else" "end" "elsif"))
  (set-repl-handler! '(ruby-mode enh-ruby-mode) #'inf-ruby)

  (when (featurep! +lsp)
    (add-hook 'enh-ruby-mode-local-vars-hook #'lsp!))

  (after! company-dabbrev-code
    (add-to-list 'company-dabbrev-code-modes 'enh-ruby-mode nil #'eq)
    (add-to-list 'company-dabbrev-code-modes 'ruby-mode nil #'eq))

  (after! inf-ruby
    ;; switch to inf-ruby from compile if we detect a breakpoint has been hit
    (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter))

  ;; so class and module pairs work
  (setq-hook! (ruby-mode enh-ruby-mode) sp-max-pair-length 6))


(use-package! robe
  :defer t
  :init
  (add-hook! 'enh-ruby-mode-hook
    (defun +ruby-init-robe-mode-maybe-h ()
      "Start `robe-mode' if `lsp-mode' isn't active."
      (unless (or (bound-and-true-p lsp-mode)
                  (bound-and-true-p lsp--buffer-deferred))
        (robe-mode +1))))
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
        "d"  #'ruby-send-definition
        "D"  #'ruby-send-definition-and-go
        "r"  #'ruby-send-region
        "R"  #'ruby-send-region-and-go
        "i"  #'ruby-switch-to-inf))


;; NOTE Must be loaded before `robe-mode'
(use-package! yard-mode
  :hook (ruby-mode enh-ruby-mode))


(use-package! rubocop
  :hook (enh-ruby-mode . rubocop-mode)
  :config
  (map! :localleader
        :map rubocop-mode-map
        "f" #'rubocop-check-current-file
        "F" #'rubocop-autocorrect-current-file
        "p" #'rubocop-check-project
        "P" #'rubocop-autocorrect-project))


;;
;;; Package & Ruby version management

(use-package! rake
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

(use-package! bundler
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
;;; Testing frameworks

(use-package! rspec-mode
  :mode ("/\\.rspec\\'" . text-mode)
  :init
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


(use-package! minitest
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
