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

  (after! dtrt-indent
    ;; `dtrt-indent' supports ruby-mode. Make it aware of enh-ruby-mode
    (add-to-list 'dtrt-indent-hook-mapping-list '(enh-ruby-mode ruby enh-ruby-indent-level)))

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
        :n "'"  #'robe-start
        ;; robe mode specific
        :n "h"  #'robe-doc
        :n "rr" #'robe-rails-refresh
        ;; inf-enh-ruby-mode
        :prefix "s"
        :n "f" #'ruby-send-definition
        :n "F" #'ruby-send-definition-and-go
        :n "r" #'ruby-send-region
        :n "R" #'ruby-send-region-and-go
        :n "i" #'ruby-switch-to-inf))


;; NOTE Must be loaded before `robe-mode'
(def-package! yard-mode
  :hook (ruby-mode enh-ruby-mode))


(def-package! rubocop
  :hook (enh-ruby-mode . rubocop-mode)
  :config
  (map! :map rubocop-mode-map
        :localleader
        :nv "f" #'rubocop-check-current-file
        :nv "F" #'rubocop-autocorrect-current-file
        :nv "p" #'rubocop-check-project
        :nv "P" #'rubocop-autocorrect-project))


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
        :n "k" #'rake
        :n "r" #'rake-rerun
        :n "R" #'rake-regenerate-cache
        :n "f" #'rake-find-task))

(def-package! bundler
  :defer t
  :init
  (map! :after enh-ruby-mode
        :localleader
        :map enh-ruby-mode-map
        :prefix "b"
        :n "c" #'bundle-check
        :n "C" #'bundle-console
        :n "i" #'bundle-install
        :n "u" #'bundle-update
        :n "e" #'bundle-exec
        :n "o" #'bundle-open))

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
  (map! :map rspec-mode-map
        :localleader
        :prefix "t"
        :n "r" #'rspec-rerun
        :n "a" #'rspec-verify-all
        :n "s" #'rspec-verify-single
        :n "v" #'rspec-verify
        :n "c" #'rspec-verify-continue
        :n "e" #'rspec-toggle-example-pendingness
        :n "f" #'rspec-verify-method
        :n "l" #'rspec-run-last-failed
        :n "m" #'rspec-verify-matching
        :n "t" #'rspec-toggle-spec-and-target-find-example
        :n "T" #'rspec-toggle-spec-and-target))


(def-package! minitest
  :defer t
  :config
  (when (featurep! :feature evil)
    (add-hook 'minitest-mode-hook #'evil-normalize-keymaps))
  (map! :map minitest-mode-map
        :localleader
        :prefix "t"
        :n "r" #'minitest-rerun
        :n "a" #'minitest-verify-all
        :n "s" #'minitest-verify-single
        :n "v" #'minitest-verify))
