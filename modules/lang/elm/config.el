;;; lang/elm/config.el -*- lexical-binding: t; -*-

(after! elm-mode
  (if (modulep! +lsp)
      (add-hook 'elm-mode-local-vars-hook #'lsp! 'append)
    (set-company-backend! 'elm-mode 'company-elm))

  (when (modulep! +tree-sitter)
    (add-hook 'elm-mode-local-vars-hook #'tree-sitter! 'append))
 
  (set-repl-handler! 'elm-mode #'run-elm-interactive)
  (set-ligatures! 'elm-mode
    :null "null"
    :true "true" :false "false"
    :int "Int" :str "String"
    :float "Float"
    :bool "Bool"
    :not "not"
    :and "&&" :or "||")

  (map! :map elm-mode-map
        :localleader
        (:prefix ("m" . "elm make")
         :desc "Compile HTML" "m" #'+elm/compile-html
         :desc "Compile HTML (optimized)" "M" #'+elm/compile-html-optimized
         :desc "Compile JS" "j" #'+elm/compile-js
         :desc "Compile JS (optimized)" "J" #'+elm/compile-js-optimized)))

(use-package! flycheck-elm
  :when (modulep! :checkers syntax -flymake)
  :after elm-mode
  :config (add-to-list 'flycheck-checkers 'elm))
