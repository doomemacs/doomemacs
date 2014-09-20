(provide 'init-ruby)

(defun enable-ruby-rsense ()
  (setq rsense-home "/usr/local/Cellar/rsense/0.3/libexec")
  (when (file-directory-p rsense-home)
    (add-to-list 'load-path (concat rsense-home "/etc"))
    (require 'rsense)
    (add-hook 'ruby-mode-hook 'ac-add-ruby-rsense)))

(defun ac-add-ruby-rsense ()
  (setq ac-sources (append '(ac-source-rsense ac-source-yasnippet) ac-sources)))

;;
(use-package ruby-mode
  :mode (("\\.rb$" . ruby-mode)
         ("\\.ru$" . ruby-mode)
         ("\\.rake$" . ruby-mode)
         ("\\.gemspec$" . ruby-mode)
         ("Gemfile$" . ruby-mode)
         ("Capfile$" . ruby-mode)
         ("Vagrantfile$" . ruby-mode)
         ("Rakefile$" . ruby-mode))
  :interpreter "ruby"
  :config
  (progn
    (use-package inf-ruby
      :config
      (progn
        (evil-set-initial-state 'inf-ruby-mode 'insert)
        (use-package ac-inf-ruby)
        (add-hook 'inf-ruby-mode-hook 'ac-inf-ruby-enable))
      :init
      (add-to-list 'ac-modes 'inf-ruby-mode))

    (use-package rspec-mode
      :defer t
      :pre-load
      (defvar evilmi-ruby-match-tags
        '((("unless" "if") ("elsif" "else") "end")
          ("begin" ("rescue" "ensure") "end")
          ("case" ("when" "else") "end")
          (("class" "def" "while" "do" "module" "for" "until") () "end")
          ;; Rake
          (("task" "namespace") () "end")
          ))
      :init
      (associate-minor-mode "_spec\\.rb\\'" rspec-mode))

	;;; Auto-completion
    ;; Remember to install rsense w/ homebrew!
    (enable-ruby-rsense)

    (add-hook! 'ruby-mode-hook
               (setq my-switch-to-repl-func 'ruby-switch-to-inf
                     my-send-region-to-repl-func 'ruby-send-region
                     my-run-code-interpreter "ruby"))

	;;; Formatting
    (setq ruby-indent-level 2)
    (setq ruby-deep-indent-paren t)
	(add-hook 'ruby-mode-hook 'enable-tab-width-2)))

(add-hook! 'find-file-hook
           (let ((rake-path
                  (f--traverse-upwards (f--exists? "Rakefile" it))))
             (when rake-path
               (use-package rake-mode)
               (rake-mode t)
               (rake-mode/visit-rakefile (expand-file-name "Rakefile" rake-path) t))))
