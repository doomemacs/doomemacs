;;; lang/dart/config.el -*- lexical-binding: t; -*-

(defun +dart-common-config (mode)
  (set-ligatures! mode
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
    :yield "yield")

  (when (modulep! +lsp)
    (add-hook (intern (format "%s-local-vars-hook" mode)) #'lsp! 'append)))

(use-package! dart-mode
  :hook (dart-mode . rainbow-delimiters-mode)
  :config
  (+dart-common-config 'dart-mode))


(use-package! dart-ts-mode
  :when (modulep! +tree-sitter)
  :defer t
  :init
  (set-tree-sitter! 'dart-mode 'dart-ts-mode
    '((dart :url "https://github.com/ast-grep/tree-sitter-dart")))
  :config
  (+dart-common-config 'dart-ts-mode))


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
          "R" #'flutter-hot-restart))
  :config
  (set-popup-rule! (concat "^" (regexp-quote flutter-buffer-name))
    :ttl 0 :quit t))


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
