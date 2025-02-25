;;; lang/elixir/config.el -*- lexical-binding: t; -*-

;; DEPRECATED: Remove when projectile is replaced with project.el
(after! projectile
  (add-to-list 'projectile-project-root-files "mix.exs"))


;;
;;; Packages

(use-package! elixir-mode
  :defer t
  :init
  ;; Disable default smartparens config. There are too many pairs; we only want
  ;; a subset of them (defined below).
  (provide 'smartparens-elixir)
  :config
  (set-ligatures! 'elixir-mode
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
  (sp-with-modes 'elixir-mode
    (sp-local-pair "do" "end"
                   :when '(("RET" "<evil-ret>"))
                   :unless '(sp-in-comment-p sp-in-string-p)
                   :post-handlers '("||\n[i]"))
    (sp-local-pair "do " " end" :unless '(sp-in-comment-p sp-in-string-p))
    (sp-local-pair "fn " " end" :unless '(sp-in-comment-p sp-in-string-p)))

  (when (modulep! +lsp +tree-sitter)
    (add-hook 'elixir-ts-mode-local-vars-hook #'lsp! 'append))

  (when (modulep! +lsp)
    (add-hook 'elixir-mode-local-vars-hook #'lsp! 'append)
    (after! lsp-mode
      (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]_build\\'")))

  (when (modulep! +tree-sitter)
    (add-hook 'elixir-mode-local-vars-hook #'tree-sitter! 'append))

  (after! highlight-numbers
    (puthash 'elixir-mode
             "\\_<-?[[:digit:]]+\\(?:_[[:digit:]]\\{3\\}\\)*\\_>"
             highlight-numbers-modelist)))


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
