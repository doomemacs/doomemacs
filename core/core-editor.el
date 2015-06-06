;;; Global editor behavior

(electric-indent-mode -1)
(setq electric-indent-chars '(?  ?: ?{))
(add-hook  'python-mode-hook 'electric-indent-local-mode)
(add-hook  'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook! 'eldoc-mode-hook (diminish 'eldoc-mode " ?"))

(setq-default fill-column 80)
(diminish 'auto-fill-function)
;; Sane scroll settings
(setq scroll-margin 5
      scroll-conservatively 9999
      scroll-preserve-screen-position t)
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

(add-hook 'help-mode-hook     'visual-line-mode)
(add-hook 'before-save-hook   'delete-trailing-whitespace)
(add-hook 'makefile-mode-hook 'narf|enable-tabs)         ; Use normal tabs in makefiles

(after "isearch" (diminish 'isearch-mode))

;; Fix code folding
;; (defun narf|init-hs-minor-mode-maybe ()
;;   (unless (bound-and-true-p hs-minor-mode)
;;     (hs-minor-mode 1)
;;     (diminish 'hs-minor-mode)))
;; (add-hook 'prog-mode-hook 'narf|init-hs-minor-mode-maybe)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package smartparens
  :diminish smartparens-mode
  :config
  (progn
    (use-package smartparens-config)
    (smartparens-global-mode 1)

    (setq blink-matching-paren t
          sp-autowrap-region nil                         ; let evil-surround handle this
          sp-highlight-pair-overlay nil
          sp-show-pair-delay 0)

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
    (sp-with-modes '(c-mode c++-mode objc-mode java-mode) ; Support for generics
      (sp-local-pair "<" ">" :when '(sp-point-after-word-p) :unless '(sp-point-before-same-p)))
    (sp-with-modes '(objc-mode scss-mode css-mode)
      (sp-local-pair "/*\n" "\n */" :post-handlers '(("||[i]" "RET"))))
    (sp-with-modes '(c-mode c++-mode php-mode java-mode)
      (sp-local-pair "/*" "" :post-handlers '((" ||\n[i]*/" "RET"))))

    (after "yasnippet" (advice-add 'yas-expand :before 'sp-remove-active-pair-overlay))))

(use-package rotate-text
  :commands (rotate-word-at-point rotate-region))

(use-package smart-forward
  :commands (smart-up smart-down smart-left smart-right))

(use-package expand-region
  :commands (er/expand-region er/contract-region er/mark-symbol er/mark-word))

(use-package hl-todo
  :commands hl-todo-mode
  :init     (add-hook 'prog-mode-hook 'hl-todo-mode))

(use-package emr
  :commands (emr-initialize emr-show-refactor-menu emr-declare-command)
  :config   (bind :map popup-menu-keymap [escape] 'keyboard-quit))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init   (add-to-hooks 'rainbow-delimiters-mode '(emacs-lisp-mode js2-mode scss-mode))
  :config (setq rainbow-delimiters-outermost-only-face-count 1))

(use-package ace-window
  :commands ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame
        aw-background nil))

(use-package ace-jump-mode
  :functions (ace-jump-char-category ace-jump-do)
  :commands (ace-jump-line-mode
             ace-jump-char-mode
             ace-jump-word-mode
             ace-jump-two-chars-mode)
  :config
  (progn
    (defun ace-jump-two-chars-mode (&optional query-char query-char-2)
      "AceJump two chars mode"
      (interactive)

      (evil-half-cursor)
      (setq query-char (or query-char (read-char ">")))
      (setq query-char-2 (or query-char-2 (read-char (concat ">" (string query-char)))))

      (if (eq (ace-jump-char-category query-char) 'other)
          (error "[AceJump] Non-printable character"))

      ;; others : digit , alpha, punc
      (setq ace-jump-query-char query-char)
      (setq ace-jump-current-mode 'ace-jump-char-mode)
      (ace-jump-do (regexp-quote (concat (char-to-string query-char)
                                         (char-to-string query-char-2)))))
    (setq ace-jump-mode-scope 'window
          ace-jump-mode-gray-background t)))

(use-package ace-link
  :commands (ace-link-info
             ace-link-help
             ace-link-compilation
             ace-link-custom
             ace-link-org)
  :init
  (progn
    (after "help-mode"
      (bind motion :map help-mode-map         "go" 'ace-link-help))
    (after "compile"
      (bind motion :map compilation-mode-map  "go" 'ace-link-compilation))
    (after "info"
      (bind motion :map Info-mode-map         "go" 'ace-link-info))
    (after "org"
      (bind motion :map org-mode-map          "go" 'ace-link-org))))

(use-package quickrun
  :commands (quickrun
             quickrun-region
             quickrun-with-arg
             quickrun-shell
             quickrun-compile-only
             quickrun-replace-region
             helm-quickrun))


(provide 'core-editor)
;;; core-editor.el ends here
