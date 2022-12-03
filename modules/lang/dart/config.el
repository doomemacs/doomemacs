;;; lang/dart/config.el -*- lexical-binding: t; -*-

(use-package! dart-mode
  :hook (dart-mode . rainbow-delimiters-mode)
  :config
  (when (modulep! +lsp)
    (add-hook 'dart-mode-local-vars-hook #'lsp! 'append))
  (set-ligatures! '(dart-mode)
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
  :when (modulep! +flutter)
  :defer t
  :init
  (map! :after dart-mode
        :map dart-mode-map
        :localleader
         (:prefix ("f" . "flutter")
          "f" #'flutter-run
          "q" #'flutter-quit
          "r" #'flutter-hot-reload
          "R" #'flutter-hot-restart)))


(use-package! lsp-dart
  :when (modulep! +lsp)
  :defer t
  :config
  (map! :map dart-mode-map
        (:localleader
         (:prefix ("t" . "test")
          "t" #'lsp-dart-run-test-at-point
          "a" #'lsp-dart-run-all-tests
          "f" #'lsp-dart-run-test-file
          "l" #'lsp-dart-run-last-test
          "v" #'lsp-dart-visit-last-test))))


(use-package! hover
  :when (modulep! +flutter)
  :defer t
  :config
  (map! :map dart-mode-map
        (:localleader
         (:prefix ("h" . "hover")
          "c" #'hover-clear-buffer
          "r" #'hover-run-or-hot-reload
          "R" #'hover-run-or-hot-restart
          "p" #'hover-take-screenshot
          "k" #'hover-kill)))
  (set-popup-rule! "\\*Hover\\*" :quit nil))
