;;; lang/sml/config.el -*- lexical-binding: t; -*-


(defun +sml-common-config (mode)
  (set-repl-handler! mode #'run-sml)
  (set-formatter! 'smlformat '("smlformat") :modes mode)

  (when (modulep! +lsp)
    (add-hook (intern (format "%s-local-vars-hook" mode)) #'lsp! 'append))

  ;; don't auto-close apostrophes (type 'a = foo) and backticks (`Foo)
  (sp-with-modes mode
    (sp-local-pair "'" nil :actions nil)
    (sp-local-pair "`" nil :actions nil))

  (map! :map ,(intern (format "%s-map" mode))
        :i "RET"   #'reindent-then-newline-and-indent
        :i "S-SPC" #'sml-electric-space
        :i "|"     #'sml-electric-pipe
        :localleader
        :desc "Run SML" "'" #'run-sml
        :prefix ("e" . "eval")
        :desc "Run buffer"                  "b" #'sml-prog-proc-send-buffer
        :desc "Run the paragraph"           "f" #'sml-send-function
        :desc "Run region"                  "r" #'sml-prog-proc-send-region))


(use-package! sml-mode
  :mode "\\.s\\(?:ml\\|ig\\)\\'"
  :config
  (+sml-common-config 'sml-mode))


(use-package! sml-ts-mode
  :when (modulep! +tree-sitter)
  :defer t
  :init
  (set-tree-sitter! 'sml-mode 'sml-ts-mode
    '((sml :url "https://github.com/MatthewFluet/tree-sitter-sml")))
  :config
  (+sml-common-config 'sml-ts-mode))


(use-package! company-mlton
  :when (modulep! :completion company)
  :hook (sml-mode . company-mlton-init)
  :hook (sml-ts-mode . company-mlton-init)
  :config
  (set-company-backend! '(sml-mode sml-ts-mode) company-mlton-grouped-backend))
