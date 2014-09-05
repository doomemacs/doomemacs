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
	;;; Ruby tools
    (use-package rbenv
      :init
      (progn
        (setq rbenv-show-active-ruby-in-modeline nil)

        (global-rbenv-mode)
        (add-hook 'ruby-mode-hook 'rbenv-use-corresponding)))

    (use-package inf-ruby
      :config
      (evil-set-initial-state 'inf-ruby-mode 'insert)
      :init
      (add-to-list 'ac-modes 'inf-ruby-mode))

    (use-package rspec-mode
      :defer t
      :config
      (progn
        (nmap rspec-mode-verifiable-keymap
              ",tr" 'rspec-rerun
              ",ta" 'rspec-verify-all
              ",ts" 'rspec-verify-single
              ",tv" 'rspec-verify)

        (nmap rspec-dired-mode-keymap
              ",tv" 'rspec-dired-verify
              ",ts" 'rspec-dired-verify-single
              ",ta" 'rspec-verify-all
              ",tr" 'rspec-rerun))
      :init
      (associate-mode "_spec\\.rb\\'" rspec-mode t))

	;;; Auto-completion
    ;; Remember to install rsense w/ homebrew!
    (enable-ruby-rsense)
    (use-package ac-inf-ruby
      :init
      (add-hook 'inf-ruby-mode-hook 'ac-inf-ruby-enable))

	;;; Formatting
    (setq ruby-indent-level 2)
    (setq ruby-deep-indent-paren nil)
	(add-hook 'ruby-mode-hook 'enable-tab-width-2)
    (require 'ruby-mode-indent-fix)

    (setq evilmi-ruby-match-tags
          '((("unless" "if") ("elsif" "else") ("end"))
            ("begin" ("rescue" "ensure") "end")
            ("case" ("when" "else") ("end"))
            (("task" "namespace" "class" "def" "while" "do" "module" "for" "until") () ("end"))
            ))

    ;; (evil-define-text-object ruby-mode-string-interp-inner (count &optional beg end type)
    ;;     "Select a string hash block in a string: #{|...|}"
    ;;     (evil-regexp-range count beg end type "#{" "}" t))
    ;; (evil-define-text-object ruby-mode-string-interp-outer (count &optional beg end type)
    ;;     "Select a string hash block in a string, including the delimiters: |#{...}|"
    ;;     (evil-regexp-range count beg end type "[#$]{" "}"))
    ;; (evil-define-key 'motion ruby-mode-map "")

	;;; Keybindings
    (nmap ruby-mode-map "gd" 'rsense-jump-to-definition)

    (run-code-with "ruby" ruby-mode-map)
    ))
