;;; lang/elm/config.el -*- lexical-binding: t; -*-

(after! elm-mode
  (add-hook 'elm-mode-hook #'rainbow-delimiters-mode)

  (set-company-backend! 'elm-mode 'company-elm)
  (set-repl-handler! 'elm-mode #'run-elm-interactive)
  (set-pretty-symbols! 'elm-mode
    :null "null"
    :true "true" :false "false"
    :int "Int" :str "String"
    :float "Float"
    :bool "Bool"

    :not "not"
    :and "&&" :or "||"))


(use-package! flycheck-elm
  :when (featurep! :checkers syntax)
  :after elm-mode
  :config (add-to-list 'flycheck-checkers 'elm nil #'eq))
