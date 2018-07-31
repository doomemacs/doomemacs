;;; lang/ruby/config.el -*- lexical-binding: t; -*-

(defvar +ruby-mode-name-functions '(+ruby-version)
  "A list of functions to retrieve a version or environment string from. The
first to return non-nil will have its result appended to the ruby-mode
`mode-name' and displayed in the mode-line.")


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
  (set-electric! 'enh-ruby-mode :words '("else" "end" "elsif"))
  (set-repl-handler! 'enh-ruby-mode #'inf-ruby) ; `inf-ruby'

  ;; so class and module pairs work
  (setq-hook! 'enh-ruby-mode-hook sp-max-pair-length 6)

  (defun +ruby|add-version-to-modeline ()
    "Add version string to the major mode in the modeline."
    (setq mode-name
          (if-let* ((result (run-hook-with-args-until-success '+ruby-mode-name-functions)))
              (format "Ruby %s" result)
            "Ruby")))
  (add-hook 'enh-ruby-mode-hook #'+ruby|add-version-to-modeline))


(def-package! yard-mode :hook enh-ruby-mode)


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


;;
;; Version managers
;;

(def-package! rbenv
  :when (featurep! +rbenv)
  :after enh-ruby-mode
  :config
  (set-env! "RBENV_ROOT")
  (when (executable-find "rbenv")
    (global-rbenv-mode +1)))


(def-package! rvm
  :when (featurep! +rvm)
  :after enh-ruby-mode)

