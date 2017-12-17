;;; lang/ruby/config.el -*- lexical-binding: t; -*-

(defvar +ruby-rbenv-versions nil
  "Available versions of ruby in rbenv.")

(defvar-local +ruby-current-version nil
  "The currently active ruby version.")


;;
;; Plugins
;;

(def-package! ruby-mode
  :mode "\\.rb$"
  :mode "\\.rake$"
  :mode "\\.gemspec$"
  :mode "\\.\\(pry\\|irb\\)rc$"
  :mode "/\\(Gem\\|Cap\\|Vagrant\\|Rake\\|Pod\\|Puppet\\|Berks\\)file$"
  :interpreter "ruby"
  :config
  (set! :company-backend 'ruby-mode '(company-dabbrev-code))
  (set! :electric 'ruby-mode :words '("else" "end" "elseif"))
  (setq ruby-deep-indent-paren t)
  ;; Don't interfere with my custom RET behavior
  (define-key ruby-mode-map [?\n] nil)

  (add-hook 'ruby-mode-hook #'flycheck-mode)

  ;; Version management with rbenv
  (defun +ruby|add-version-to-modeline ()
    "Add version string to the major mode in the modeline."
    (setq mode-name
          (if +python-current-version
              (format "Ruby %s" +ruby-current-version)
            "Ruby")))
  (add-hook 'ruby-mode-hook #'+ruby|add-version-to-modeline)

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
    (add-hook 'ruby-mode-hook #'+ruby|detect-rbenv-version))

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
(def-package! yard-mode :hook ruby-mode)


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


(def-package! rake
  :commands (rake rake-find-task rake-rerun)
  :config (setq rake-completion-system 'default))

