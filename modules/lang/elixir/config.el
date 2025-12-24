;;; lang/elixir/config.el -*- lexical-binding: t; -*-

;; DEPRECATED: Remove when projectile is replaced with project.el
(after! projectile
  (add-to-list 'projectile-project-root-files "mix.exs"))


;;
;;; Packages

(defun +elixir-common-config (mode)
  ;; ...and only complete the basics
  (sp-with-modes mode
    (sp-local-pair "do" "end"
                   :when '(("RET" "<evil-ret>"))
                   :unless '(sp-in-comment-p sp-in-string-p)
                   :post-handlers '("||\n[i]"))
    (sp-local-pair "do " " end" :unless '(sp-in-comment-p sp-in-string-p))
    (sp-local-pair "fn " " end" :unless '(sp-in-comment-p sp-in-string-p)))

  (when (modulep! +lsp)
    (add-hook (intern (format "%s-local-vars-hook" mode)) #'lsp! 'append))

  (use-package! flycheck-credo
    :when (modulep! :checkers syntax -flymake)
    :config (flycheck-credo-setup))

  (use-package! exunit
    :defer t
    :init
    (add-hook (intern (format "%s-hook" mode)) #'exunit-mode)
    (map! :localleader
          :map ,(intern (format "%s-map" mode))
          :prefix ("t" . "test")
          "a" #'exunit-verify-all
          "r" #'exunit-rerun
          "v" #'exunit-verify
          "T" #'exunit-toggle-file-and-test
          "t" #'exunit-toggle-file-and-test-other-window
          "s" #'exunit-verify-single)))


(use-package! elixir-mode
  :defer t
  :init
  ;; Disable default smartparens config. There are too many pairs; we only want
  ;; a subset of them (defined below).
  (provide 'smartparens-elixir)

  (when (modulep! +lsp)
    (after! lsp-mode
      (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]_build\\'"))
    (after! lsp-elixir
      ;; HACK: lsp-elixir is hardcoded to use the server `lsp-install-server'
      ;;   installs, ignoring any system-provided executables. This fixes that,
      ;;   so long as the user hasn't changed `lsp-elixir-server-command'
      ;;   themselves,
      (when (and (member lsp-elixir-server-command
                         '(("language_server.bat")
                           ("language_server.sh")))
                 (executable-find "elixir-ls"))
        (setq lsp-elixir-server-command '("elixir-ls")))))
  :config
  (+elixir-common-config 'elixir-mode))


(use-package! elixir-ts-mode  ; 30.1+ only
  :when (modulep! +tree-sitter)
  :defer t
  :init
  (set-tree-sitter! 'elixir-mode 'elixir-ts-mode
    '((elixir :url "https://github.com/elixir-lang/tree-sitter-elixir"
              :commit "d24cecee673c4c770f797bac6f87ae4b6d7ddec5")
      (heex :url "https://github.com/phoenixframework/tree-sitter-heex"
            :commit "b5a7cb5f74dc695a9ff5f04919f872ebc7a895e9")))
  :config
  (+elixir-common-config 'elixir-ts-mode))


(use-package! heex-ts-mode
  :when (modulep! +tree-sitter)
  :when (fboundp 'heex-ts-mode) ; 30.1+ only
  :mode "\\.[hl]?eex\\'")
