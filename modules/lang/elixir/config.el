;;; lang/elixir/config.el -*- lexical-binding: t; -*-

;; DEPRECATED: Remove when projectile is replaced with project.el
(after! projectile
  (add-to-list 'projectile-project-root-files "mix.exs"))


;;
;;; Packages

(defun +elixir-common-config (mode)
  (set-ligatures! mode
    ;; Functional
    :def "def"
    :lambda "fn"
    ;; :src_block "do"
    ;; :src_block_end "end"
    ;; Flow
    :not "!"
    :in "in" :not-in "not in"
    :and "and" :or "or"
    :for "for"
    :return "return" :yield "use")

  ;; ...and only complete the basics
  (sp-with-modes mode
    (sp-local-pair "do" "end"
                   :when '(("RET" "<evil-ret>"))
                   :unless '(sp-in-comment-p sp-in-string-p)
                   :post-handlers '("||\n[i]"))
    (sp-local-pair "do " " end" :unless '(sp-in-comment-p sp-in-string-p))
    (sp-local-pair "fn " " end" :unless '(sp-in-comment-p sp-in-string-p)))

  (when (modulep! +lsp)
    (add-hook (intern (format "%s-local-vars-hook" mode)) #'lsp! 'append)))


(use-package! elixir-mode
  :defer t
  :init
  ;; Disable default smartparens config. There are too many pairs; we only want
  ;; a subset of them (defined below).
  (provide 'smartparens-elixir)

  (when (modulep! +lsp)
    (after! lsp-mode
      (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]_build\\'"))))


(use-package! elixir-ts-mode
  :when (modulep! +tree-sitter)
  :when (fboundp 'elixir-ts-mode) ; 30.1+ only
  :defer t
  :init
  (set-tree-sitter! 'elixir-mode 'elixir-ts-mode
    '((elixir :url "https://github.com/elixir-lang/tree-sitter-elixir"
              :rev "v0.3.3")
      (heex :url "https://github.com/phoenixframework/tree-sitter-heex"
            :rev "v0.7.0")))
  :config
  ;; HACK: Rely on `major-mode-remap-defaults' (and elixir-mode's autoloaded
  ;;   auto-mode-alist entries).
  (cl-callf2 rassq-delete-all 'elixir-ts-mode auto-mode-alist)

  (+elixir-common-config 'elixir-ts-mode))


(use-package! heex-ts-mode
  :when (modulep! +tree-sitter)
  :when (fboundp 'heex-ts-mode) ; 30.1+ only
  :mode "\\.[hl]?eex\\'")


(use-package! flycheck-credo
  :when (modulep! :checkers syntax -flymake)
  :after elixir-mode
  :config (flycheck-credo-setup))


(use-package! exunit
  :hook (elixir-mode . exunit-mode)
  :init
  (map! :after elixir-mode
        :localleader
        :map elixir-mode-map
        :prefix ("t" . "test")
        "a" #'exunit-verify-all
        "r" #'exunit-rerun
        "v" #'exunit-verify
        "T" #'exunit-toggle-file-and-test
        "t" #'exunit-toggle-file-and-test-other-window
        "s" #'exunit-verify-single))
