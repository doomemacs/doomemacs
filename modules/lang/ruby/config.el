;;; lang/ruby/config.el -*- lexical-binding: t; -*-

(defvar +ruby-rbenv-versions nil
  "Available versions of ruby in rbenv.")

(defvar-local +ruby-current-version nil
  "The currently active ruby version.")


;;
;; Plugins
;;

(def-package! enh-ruby-mode
  :mode "\\.rb\\'"
  :mode "\\.rake\\'"
  :mode "\\.gemspec\\'"
  :mode "\\.\\(?:pry\\|irb\\)rc\\'"
  :mode "/\\(?:Gem\\|Cap\\|Vagrant\\|Rake\\|Pod\\|Puppet\\|Berks\\)file\\'"
  :config
  (set-env! "RBENV_ROOT")
  (set-electric! 'enh-ruby-mode :words '("else" "end" "elsif"))
  (set-repl-handler! 'enh-ruby-mode #'inf-ruby) ; `inf-ruby'

  ;; so class and module pairs work
  (setq-hook! 'enh-ruby-mode-hook sp-max-pair-length 6)

  ;; Version management with rbenv
  (defun +ruby|add-version-to-modeline ()
    "Add version string to the major mode in the modeline."
    (setq mode-name
          (if +ruby-current-version
              (format "Ruby %s" +ruby-current-version)
            "Ruby")))
  (add-hook 'enh-ruby-mode-hook #'+ruby|add-version-to-modeline)

  (if (not (executable-find "rbenv"))
      (setq-default +ruby-current-version (string-trim (shell-command-to-string "ruby --version 2>&1 | cut -d' ' -f2")))
    (setq +ruby-rbenv-versions (split-string (shell-command-to-string "rbenv versions --bare") "\n" t))

    (defun +ruby|detect-rbenv-version ()
      "Detect the rbenv version for the current project and set the relevant
environment variables."
      (when-let* ((version-str (shell-command-to-string "RBENV_VERSION= ruby --version 2>&1 | cut -d' ' -f2")))
        (setq version-str (string-trim version-str)
              +ruby-current-version version-str)
        (when (member version-str +ruby-rbenv-versions)
          (setenv "RBENV_VERSION" version-str))))
    (add-hook 'enh-ruby-mode-hook #'+ruby|detect-rbenv-version)))


(def-package! yard-mode :hook enh-ruby-mode)


(def-package! rbenv
  :after enh-ruby-mode
  :config
  (when (executable-find "rbenv")
    (global-rbenv-mode +1)))


(def-package! rubocop
  :hook (enh-ruby-mode . rubocop-mode)
  :config
  (map! :map rubocop-mode-map
        :localleader
        :nv "f" #'rubocop-check-current-file
        :nv "F" #'rubocop-autocorrect-current-file
        :nv "p" #'rubocop-check-project
        :nv "P" #'rubocop-autocorrect-project))


(def-package! robe
  :hook (enh-ruby-mode . robe-mode)
  :init
  ;; robe-start errors if you hit no.
  (defun +ruby|init-robe ()
    (when (executable-find "ruby")
      (cl-letf (((symbol-function #'yes-or-no-p) (lambda (_) t)))
        (save-window-excursion
          (ignore-errors (robe-start)))
        (when (robe-running-p)
          (add-hook 'kill-buffer-hook #'+ruby|cleanup-robe-servers nil t)))))
  (add-hook 'enh-ruby-mode-hook #'+ruby|init-robe)
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
  (advice-remove 'compilation-buffer-name #'rspec-compilation-buffer-name-wrapper)
  :config
  (remove-hook 'enh-ruby-mode-hook #'rspec-enable-appropriate-mode)
  (map! :map (rspec-mode-map rspec-verifiable-mode-map)
        :localleader
        :prefix "t"
        :n "r" #'rspec-rerun
        :n "a" #'rspec-verify-all
        :n "s" #'rspec-verify-single
        :n "v" #'rspec-verify)

  ;; Evil integration
  (when (featurep! :feature evil +everywhere)
    (add-hook! '(rspec-mode-hook rspec-verifiable-mode-hook)
      #'evil-normalize-keymaps)))


(def-package! company-inf-ruby
  :when (featurep! :completion company)
  :after inf-ruby
  :config (set-company-backend! 'inf-ruby-mode 'company-inf-ruby))

