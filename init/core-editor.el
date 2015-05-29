;; Global editor behavior
(electric-indent-mode -1)
(setq electric-indent-chars '(?  ?: ?{))
(add-hook 'python-mode-hook 'electric-indent-local-mode)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook! 'eldoc-mode-hook (diminish 'eldoc-mode " ?"))

(setq-default fill-column 80)
(diminish 'auto-fill-function)
;; Sane scroll settings
(setq scroll-margin 5)
(setq scroll-conservatively 9999)
(setq scroll-preserve-screen-position 1)
;; I'll use visual mode, kthxbai
(setq shift-select-mode nil)

;;;; Modes 'n hooks ;;;;;;;;;;;;;;;;;
(associate-mode "/LICENSE[^/]*$"                     'text-mode)
(associate-mode "zsh\\(env\\|rc\\)?$"                'sh-mode)
(associate-mode "z\\(profile\\|login\\|logout\\)?$"  'sh-mode)
(associate-mode "zsh/"                               'sh-mode)
(associate-mode "\\.applescript$"                    'applescript-mode)
(associate-mode "Cask$"                              'emacs-lisp-mode)
(associate-mode "\\.el\\.gz$"                        'emacs-lisp-mode)
(associate-mode "/Makefile$"                         'makefile-gmake-mode)
(associate-mode "\\.plist$"                          'nxml-mode)

(add-hook 'help-mode-hook 'visual-line-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; (setenv "SHELL" (s-trim (shell-command-to-string "which zsh")))
(setenv "SHELL" "/usr/local/bin/zsh")
(setenv "EMACS" "1")

;; show-paren faces
(set-face-background 'show-paren-match nil)
(set-face-foreground 'show-paren-match "orange")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)
(setq show-paren-delay 0)

(let ((face 'evil-search-highlight-persist-highlight-face))
  (set-face-attribute face nil :inherit 'isearch-lazy-highlight-face)
  (set-face-foreground face nil)
  (set-face-background face nil))

(diminish 'isearch-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package anzu
  :diminish anzu-mode
  :config (global-anzu-mode +1))

(use-package smartparens
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)

    (setq blink-matching-paren t)
    (setq sp-autowrap-region nil            ; let evil-surround handle this
          sp-highlight-pair-overlay nil
          sp-show-pair-delay 0
          sp-autoescape-string-quote nil)

    ;; Handle newlines + spaces
    (sp-pair "{" "}" :post-handlers '(("||\n[i]" "RET") ("| " " ")) :unless '(sp-point-before-word-p sp-point-before-same-p))
    (sp-pair "(" ")" :post-handlers '(("||\n[i]" "RET") ("| " " ")) :unless '(sp-point-before-word-p sp-point-before-same-p))

    ;; Auto-close more conservatively
    (sp-pair "[" nil  :unless '(sp-point-before-word-p sp-point-before-same-p))
    (sp-pair "'" nil  :unless '(sp-point-after-word-p sp-point-before-word-p sp-point-before-same-p))
    (sp-pair "\"" nil :unless '(sp-point-after-word-p sp-point-before-word-p sp-point-before-same-p))

    (sp-with-modes '(json-mode js2-mode ruby-mode enh-ruby-mode python-mode)
      (sp-local-pair "[" nil :post-handlers '(("||\n[i]" "RET"))))

    (sp-with-modes '(c-mode c++-mode objc-mode java-mode scss-mode css-mode php-mode)
      (sp-local-pair "/* " " */" :post-handlers '(("||\n[i]" "RET")))
      (sp-local-pair "/**" "*/" :post-handlers '(("||\n[i]" "RET"))))

    ;; Support for generics
    (sp-with-modes '(c-mode c++-mode objc-mode java-mode)
      (sp-local-pair "<" ">" :when '(sp-point-after-word-p) :unless '(sp-point-before-same-p)))

    (sp-with-modes '(objc-mode scss-mode css-mode)
      (sp-local-pair "/*\n" "\n */" :post-handlers '(("||[i]" "RET"))))

    (sp-with-modes '(c-mode c++-mode php-mode java-mode)
      (sp-local-pair "/*" "" :post-handlers '((" ||\n[i]*/" "RET"))))

    (after "yasnippet"
      (defadvice yas-expand (before advice-for-yas-expand activate)
        (sp-remove-active-pair-overlay)))))

(use-package rotate-text
  :commands (rotate-word-at-point rotate-region))

(use-package smart-forward
  :commands (smart-up smart-down smart-left smart-right))

(use-package expand-region
  :commands (er/expand-region er/contract-region er/mark-symbol er/mark-word))

(use-package hl-todo
  :commands hl-todo-mode
  :init (add-hook 'prog-mode-hook 'hl-todo-mode))

(use-package emr
  :commands (emr-initialize emr-show-refactor-menu)
  :config   (bind popup-menu-keymap [escape] 'keyboard-quit))

(use-package dash-at-point
  :if is-mac
  :commands (dash-at-point dash-at-point-with-docset)
  :config
  (add-to-list 'dash-at-point-mode-alist
               '(java-mode . "java,droid,javafx,grails,groovy,playjava,spring,cvj,processing,javadoc")))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init (add-hooks '(emacs-lisp-mode-hook js2-mode-hook scss-mode-hook)
                   'rainbow-delimiters-mode)
  :config
  (progn
    (setq rainbow-delimiters-outermost-only-face-count 1)
    (set-face-attribute 'rainbow-delimiters-depth-1-face nil
                        :foreground 'unspecified
                        :inherit 'my-outermost-paren-face)))

(use-package yaml-mode
  :mode "\\.ya?ml$"
  :init (add-hook 'yaml-mode-hook 'enable-tab-width-2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(progn ; Code building
  (defvar my-build-command '("make %s" . "Makefile"))
  (make-variable-buffer-local 'my-build-command)

  (defun set-build-command (command &optional file)
    (when (or (null file)
              (project-has-files file))
      (setq my-build-command `(,command . ,file))))

  (evil-define-command my:build (arg)
    "Call a build command in the current directory.
If ARG is nil this function calls `recompile', otherwise it calls
`compile' passing ARG as build command."
    (interactive "<sh>")
    (when (null my-build-command)
      (user-error "No build command was set"))
    (let ((build-file (cdr my-build-command))
          (build-cmd (car my-build-command)))
      (if (project-has-files build-file)
          (compile (format "cd '%s' && %s" build-file (format build-cmd (or arg ""))))
        (error "Could not find Makefile")))))

(progn ; Code running
  (evil-define-operator my:eval-region (beg end)
    :move-point nil
    (interactive "<r>")
    (cond ((eq major-mode 'emacs-lisp-mode)
           (eval-region beg end))
          (t
           (let ((interp (my--get-interpreter))
                 (max-mini-window-height 1))
             (when interp (shell-command-on-region beg end interp))))))

  (evil-define-command my:eval-buffer ()
    (interactive)
    (cond ((eq major-mode 'emacs-lisp-mode)
           (eval-buffer))
          (t
           (let ((interp (my--get-interpreter))
                 (max-mini-window-height 1))
             (when interp (shell-command-on-region (point-min) (point-max) interp))))))

  (defun my--get-interpreter ()
    (car (--first (eq (cdr it) major-mode) interpreter-mode-alist))))


(provide 'core-editor)
;;; core-editor.el ends here
