;;; lang/ruby/config.el -*- lexical-binding: t; -*-

(defvar +ruby-mode-line-indicator '("" +ruby--version)
  "Format for the ruby version/env indicator in the mode-line.")

(defvar-local +ruby--version nil
  "The ruby version in the current buffer.")


;;
;; Packages

(def-package! enh-ruby-mode
  :mode ("\\.\\(?:pry\\|irb\\)rc\\'" . +ruby|init)
  :mode ("\\.\\(?:rb\\|rake\\|rabl\\|ru\\|builder\\|gemspec\\|jbuilder\\|thor\\)\\'" .  +ruby|init)
  :mode ("/\\(?:Berks\\|Cap\\|Gem\\|Guard\\|Pod\\|Puppet\\|Rake\\|Thor\\|Vagrant\\)file\\'" .  +ruby|init)
  :preface
  (after! ruby-mode (require 'enh-ruby-mode))
  (defun +ruby|init ()
    "Enable `enh-ruby-mode' if ruby is available, otherwise `ruby-mode'."
    (if (executable-find "ruby")
        (enh-ruby-mode)
      (ruby-mode)))
  :config
  (set-env! "RBENV_ROOT")
  (set-electric! '(ruby-mode enh-ruby-mode) :words '("else" "end" "elsif"))
  (set-repl-handler! '(ruby-mode enh-ruby-mode) #'inf-ruby)

  (after! company-dabbrev-code
    (add-to-list 'company-dabbrev-code-modes 'enh-ruby-mode nil #'eq)
    (add-to-list 'company-dabbrev-code-modes 'ruby-mode nil #'eq))

  ;; so class and module pairs work
  (setq-hook! (ruby-mode enh-ruby-mode) sp-max-pair-length 6)

  ;; Add ruby version string to the major mode in the modeline
  (defun +ruby|adjust-mode-line ()
    (setq mode-name +ruby-mode-line-indicator))
  (add-hook 'enh-ruby-mode-hook #'+ruby|adjust-mode-line)

  (add-hook 'enh-ruby-mode-hook #'+ruby|update-version))


(def-package! robe
  :hook (enh-ruby-mode . robe-mode)
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

;; `rvm'
(setq rspec-use-rvm t)

(after! rbenv
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

  (if (featurep! :feature evil)
      (add-hook 'rspec-mode-hook #'evil-normalize-keymaps)
    (setq rspec-verifiable-mode-keymap (make-sparse-keymap)
          rspec-mode-keymap (make-sparse-keymap)))
  :config
  (map! :localleader
        :map rspec-mode-map
        :prefix "t"
        "r" #'rspec-rerun
        "a" #'rspec-verify-all
        "s" #'rspec-verify-single
        "v" #'rspec-verify
        "c" #'rspec-verify-continue
        "e" #'rspec-toggle-example-pendingness
        "f" #'rspec-verify-method
        "l" #'rspec-run-last-failed
        "m" #'rspec-verify-matching
        "t" #'rspec-toggle-spec-and-target-find-example
        "T" #'rspec-toggle-spec-and-target))


(def-package! minitest
  :defer t
  :config
  (when (featurep! :feature evil)
    (add-hook 'minitest-mode-hook #'evil-normalize-keymaps))
  (map! :localleader
        :map minitest-mode-map
        :prefix "t"
        "r" #'minitest-rerun
        "a" #'minitest-verify-all
        "s" #'minitest-verify-single
        "v" #'minitest-verify))
