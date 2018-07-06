;;; lang/ruby/config.el -*- lexical-binding: t; -*-

(defvar +ruby-rbenv-versions nil
  "Available versions of ruby in rbenv.")

(defvar-local +ruby-current-version nil
  "The currently active ruby version.")

(defvar +ruby-ask-for-server t
  "Ask for a server whenever you open a ruby buffer.

This will only ask once if you say yes, but if you say no and keep opening
buffers, itll ask every time.")

;; FIXME: Add these?
;; does anyone actually use these?
;; (map! :map ruby-mode-map
;;       :localleader
;;       :prefix "r"
;;       :nv "b"  #'ruby-toggle-block
;;       :nv "ec" #'ruby-refactor-extract-constant
;;       :nv "el" #'ruby-refactor-extract-to-let
;;       :nv "em" #'ruby-refactor-extract-to-method
;;       :nv "ev" #'ruby-refactor-extract-local-variable
;;       :nv "ad" #'ruby-refactor-add-parameter
;;       :nv "cc" #'ruby-refactor-convert-post-conditional))
(def-package! enh-ruby-mode
  :mode "\\.rb$"
  :mode "\\.rake$"
  :mode "\\.gemspec$"
  :mode "\\.\\(pry\\|irb\\)rc$"
  :mode "/\\(Gem\\|Cap\\|Vagrant\\|Rake\\|Pod\\|Puppet\\|Berks\\)file$"
  :config
  (set-env! "RBENV_ROOT")
  (add-hook 'enh-ruby-mode-hook #'flycheck-mode)
  (set-electric! 'enh-ruby-ode :words '("else" "end" "elsif"))
  (setq sp-max-pair-length 6) ;; so class and module work
  (set-repl-handler! 'enh-ruby-mode #'inf-ruby) ; `inf-ruby'

  ;; FIXME: needed??
  (after! smartparens-ruby
    (sp-local-pair 'enh-ruby-mode "{" "}"
                  :pre-handlers '(:rem sp-ruby-pre-handler)
                  :post-handlers '(:rem sp-ruby-post-handler)))
  ;; Version management with rbenv
  (defun +ruby|add-version-to-modeline ()
    "Add version string to the major mode in the modeline."
    (setq mode-name
          (if +ruby-current-version
              (format "Ruby %s" +ruby-current-version)
            "Ruby")))
  (add-hook 'enh-ruby-mode-hook #'+ruby|add-version-to-modeline)

  (if (not (executable-find "rbenv"))
      (setq +ruby-current-version (string-trim (shell-command-to-string "ruby --version 2>&1 | cut -d' ' -f2")))
    (setq +ruby-rbenv-versions (split-string (shell-command-to-string "rbenv versions --bare") "\n" t))

    (defun +ruby|detect-rbenv-version ()
      "Detect the rbenv version for the current project and set the relevant
environment variables."
      (when-let* ((version-str (shell-command-to-string "ruby --version 2>&1 | cut -d' ' -f2")))
        (setq version-str (string-trim version-str)
              +ruby-current-version version-str)
        (when (member version-str +ruby-rbenv-versions)
          (setenv "RBENV_VERSION" version-str))))
    (add-hook 'enh-ruby-mode-hook #'+ruby|detect-rbenv-version)))

(def-package! yard-mode :hook enh-ruby-mode)

(def-package! rbenv
  :after enh-ruby-mode
  :when (executable-find "rbenv")
  :config
  (global-rbenv-mode))

(def-package! rubocop
  :hook (enh-ruby-mode . rubocop-mode))

;; FIXME: Clean up all processes from this/inf-ruby when all the ruby buffers
;; are closed
(def-package! robe
  :hook (enh-ruby-mode . robe-mode)
  :init
  ;; robe-start erros if you hit no.
  ;; FIXME: Once hit no, itll ask every time you open a new buffer. This is
  ;; defined behaviour but not what we want!
  (when +ruby-ask-for-server
    (add-hook! 'enh-ruby-mode-hook (ignore-errors (call-interactively #'robe-start))))
  :config
  (set-company-backend! 'robe-mode 'company-robe))


(def-package! rspec-mode
  :mode ("/\\.rspec\\'" . text-mode)
  :init
  (associate! rspec-mode :match "/\\.rspec$")
  (associate! rspec-mode :modes (enh-ruby-mode yaml-mode) :files ("spec/"))

  (defvar evilmi-ruby-match-tags
    '((("unless" "if") ("elsif" "else") "end")
      ("begin" ("rescue" "ensure") "end")
      ("case" ("when" "else") "end")
      (("class" "def" "while" "do" "module" "for" "until") () "end")
      ;; Rake
      (("task" "namespace") () "end")))

  ;; This package autoloads this advice, but does not autoload the advice
  ;; function, causing void-symbol errors when using the compilation buffer
  ;; (even for things unrelated to ruby/rspec). Even if the function were
  ;; autoloaded, it seems silly to add this advice before rspec-mode is loaded,
  ;; so remove it anyway!
  (advice-remove 'compilation-buffer-name 'rspec-compilation-buffer-name-wrapper)
  :config
  (remove-hook 'enh-ruby-mode-hook #'rspec-enable-appropriate-mode)
  (map! :map (rspec-mode-map rspec-verifiable-mode-map)
        :localleader
        :prefix "t"
        :n "r" #'rspec-rerun
        :n "a" #'rspec-verify-all
        :n "s" #'rspec-verify-single
        :n "v" #'rspec-verify))

(def-package! company-inf-ruby
  :when (featurep! :completion company)
  :after inf-ruby
  :config (set-company-backend! 'inf-ruby-mode 'company-inf-ruby))

;;
;; Evil integration
;;

(when (featurep! :feature evil +everywhere)
  (add-hook! '(rspec-mode-hook rspec-verifiable-mode-hook)
    #'evil-normalize-keymaps))
