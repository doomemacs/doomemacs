;;; lang/sml/config.el -*- lexical-binding: t; -*-

(use-package! sml-mode
  :mode "\\.s\\(?:ml\\|ig\\)\\'"
  :config
  (set-repl-handler! '(sml-mode sml-ts-mode) #'run-sml)
  (set-formatter! 'smlformat '("smlformat") :modes '(sml-mode sml-ts-mode))

  (when (modulep! +lsp)
    (add-hook 'sml-mode-local-vars-hook #'lsp! 'append))

  ;; don't auto-close apostrophes (type 'a = foo) and backticks (`Foo)
  (sp-with-modes '(sml-mode sml-ts-mode)
    (sp-local-pair "'" nil :actions nil)
    (sp-local-pair "`" nil :actions nil))

  (map! :map sml-mode-map
        :i "RET"   #'reindent-then-newline-and-indent
        :i "S-SPC" #'sml-electric-space
        :i "|"     #'sml-electric-pipe
        :localleader
        :desc "Run SML" "'" #'run-sml
        :prefix ("e" . "eval")
        :desc "Run buffer"                  "b" #'sml-prog-proc-send-buffer
        :desc "Run the paragraph"           "f" #'sml-send-function
        :desc "Run region"                  "r" #'sml-prog-proc-send-region))


;; TODO: Mirror sml-mode keybinds to ts-mode
(use-package! sml-ts-mode
  :when (modulep! +tree-sitter)
  :when (fboundp 'treesit-available-p)
  :defer t
  :init
  (set-tree-sitter! 'sml-mode 'sml-ts-mode
    '((sml :url "https://github.com/MatthewFluet/tree-sitter-sml")))
  :config
  (when (modulep! +lsp)
    (add-hook 'sml-ts-mode-local-vars-hook #'lsp! 'append)))


(use-package! company-mlton
  :when (modulep! :completion company)
  :hook (sml-mode . company-mlton-init)
  :config
  (set-company-backend! 'sml-mode company-mlton-grouped-backend))
