;;; lang/dart/config.el -*- lexical-binding: t; -*-

(use-package! dart-mode
  :when (featurep! +lsp)
  :hook (dart-mode-local-vars . lsp!)
  :config
  (when (and (featurep! +flutter) IS-LINUX)
    (when-let (path (doom-glob "/opt/flutter/bin/cache/dart-sdk"))
      (setq flutter-sdk-path path)))
  (set-pretty-symbols! '(dart-mode)
    ;; Functional
    :def "Function"
    :lambda "() =>"
    ;; Types
    :null "null"
    :true "true" :false "false"
    :int "int" :float "double"
    :str "String"
    :bool "bool"
    :list "List"
    ;; Flow
    :not "!"
    :in "in"
    :and "&&" :or "||"
    :for "for"
    :return "return"
    ;; Other
    :yield "yield"))


(use-package! flutter
  :when (featurep! +flutter)
  :defer t
  :init
  (map! :map dart-mode-map
        :localleader
        "r" #'flutter-run-or-hot-reload))


(use-package! hover
  :when (featurep! +flutter)
  :defer t
  :config
  (map! :map dart-mode-map
        :localleader
        "h r" #'hover-run-or-hot-reload
        "h R" #'hover-run-or-hot-restart))
