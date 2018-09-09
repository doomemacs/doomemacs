;;; lang/ruby/config.el -*- lexical-binding: t; -*-

(defvar +ruby-mode-line-indicator
  '("Ruby" (+ruby-version (" " +ruby-version)))
  "Format for the ruby version/env indicator in the mode-line.")

(defvar-local +ruby-version nil
  "The ruby version in the current buffer.")


;;
;; Packages

(def-package! enh-ruby-mode
  :mode "\\.rb\\'"
  :mode "\\.rake\\'"
  :mode "\\.gemspec\\'"
  :mode "\\.\\(?:pry\\|irb\\)rc\\'"
  :mode "/\\(?:Gem\\|Cap\\|Vagrant\\|Rake\\|Pod\\|Puppet\\|Berks\\)file\\'"
  :config
  (set-electric! 'enh-ruby-mode :words '("else" "end" "elsif"))
  (set-repl-handler! 'enh-ruby-mode #'inf-ruby) ; `inf-ruby'

  (after! dtrt-indent
    ;; `dtrt-indent' supports ruby-mode. Make it aware of enh-ruby-mode
    (add-to-list 'dtrt-indent-hook-mapping-list '(enh-ruby-mode ruby enh-ruby-indent-level)))
  ;; so class and module pairs work
  (setq-hook! 'enh-ruby-mode-hook sp-max-pair-length 6)

  ;; Add ruby version string to the major mode in the modeline
  (defun +ruby|adjust-mode-line ()
    (setq mode-name +ruby-mode-line-indicator))
  (add-hook 'enh-ruby-mode-hook #'+ruby|adjust-mode-line)

  (defun +ruby|update-version (&rest _)
    (setq +ruby-version (+ruby-version)))
  (+ruby|update-version)
  (add-hook 'enh-ruby-mode-hook #'+ruby|update-version))


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
          (with-demoted-errors "ROBE ERROR: %s"
            (robe-start)))
        (when (robe-running-p)
          (add-hook 'kill-buffer-hook #'+ruby|cleanup-robe-servers nil t)))))
  (add-hook 'enh-ruby-mode-hook #'+ruby|init-robe)
  :config
  (set-company-backend! 'robe-mode 'company-robe))


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

  (unless (featurep! :feature evil)
    (setq rspec-verifiable-mode-keymap (make-sparse-keymap)
          rspec-mode-keymap (make-sparse-keymap)))

  (defun +ruby*init-appropriate-rspec-mode ()
    "TODO"
    (cond ((rspec-buffer-is-spec-p)
           (rspec-mode +1))
          ((let ((proot (doom-project-root 'nocache)))
             (or (file-directory-p (expand-file-name "spec" proot))
                 (file-exists-p (expand-file-name ".rspec" proot))))
           (rspec-verifiable-mode +1))))
  (advice-add #'rspec-enable-appropriate-mode :override #'+ruby*init-appropriate-rspec-mode)
  :config
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

(def-package! rbenv
  :when (featurep! +rbenv)
  :after enh-ruby-mode
  :config (set-env! "RBENV_ROOT"))


(def-package! rvm
  :when (featurep! +rvm)
  :after enh-ruby-mode)

