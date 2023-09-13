;;; lang/purescript/config.el -*- lexical-binding: t; -*-

(after! purescript-mode
  (when (modulep! +lsp)
    (add-hook 'purescript-mode-local-vars-hook #'lsp! 'append))

  (add-hook! 'purescript-mode-hook
             #'purescript-indentation-mode
             #'rainbow-delimiters-mode)

  (set-formatter! 'purs-tidy '("purs-tidy" "format") :modes '(purescript-mode))

  (map! :localleader
        :map purescript-mode-map
        "t" #'psc-ide-show-type
        "c" #'psc-ide-case-split
        "i" #'psc-ide-add-import
        "a" #'psc-ide-add-clause
        "f" #'psc-ide-flycheck-insert-suggestion
        (:prefix ("l" . "load")
         "m" #'psc-ide-load-module
         "a" #'psc-ide-load-all)
        (:prefix ("s" . "server")
         "s" #'psc-ide-server-start
         "S" #'psc-ide-server-quit))

  (set-repl-handler! 'purescript-mode #'psci)

  (set-lookup-handlers! 'purescript-mode
    :definition #'psc-ide-goto-definition
    :documentation #'purescript-pursuit))


(use-package! psc-ide
  :hook (purescript-mode . psc-ide-mode)
  :config
  (remove-hook 'company-backends 'company-psc-ide-backend)
  (when (and (modulep! :checkers syntax)
             (not (modulep! :checkers syntax +flymake)))
    (psc-ide-flycheck-setup))
  (set-company-backend! 'purescript-mode 'company-psc-ide-backend))
