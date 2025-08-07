;;; lang/javascript/config.el -*- lexical-binding: t; -*-

(after! projectile
  (pushnew! projectile-project-root-files "package.json")
  (pushnew! projectile-globally-ignored-directories "node_modules" "flow-typed"))


;;
;;; Major modes

(dolist (feature '(typescript-ts-mode
                   tsx-ts-mode
                   (nodejs-repl-mode . nodejs-repl)))
  (let ((pkg  (or (cdr-safe feature) feature))
        (mode (or (car-safe feature) feature)))
    (with-eval-after-load pkg
      (set-docsets! mode "JavaScript"
        "AngularJS" "Backbone" "BackboneJS" "Bootstrap" "D3JS" "EmberJS" "Express"
        "ExtJS" "JQuery" "JQuery_Mobile" "JQuery_UI" "KnockoutJS" "Lo-Dash"
        "MarionetteJS" "MomentJS" "NodeJS" "PrototypeJS" "React" "RequireJS"
        "SailsJS" "UnderscoreJS" "VueJS" "ZeptoJS")
      (set-ligatures! mode
        ;; Functional
        :def "function"
        :lambda "() =>"
        :composition "compose"
        ;; Types
        :null "null"
        :true "true" :false "false"
        ;; Flow
        :not "!"
        :and "&&" :or "||"
        :for "for"
        :return "return"
        ;; Other
        :yield "import"))))

(defun +javascript-common-config (mode &optional langs)
  (let ((mode-vars-hook (intern (format "%s-local-vars-hook" mode)))
        (mode-hook (intern (format "%s-hook" mode))))
    (when (modulep! +lsp)
      (add-hook mode-vars-hook #'lsp! 'append))
    (set-repl-handler! mode #'+javascript/open-repl)
    (add-hook mode-hook (lambda () (when langs
      (tree-sitter-ensure-installed! langs))) 'append)))

(use-package! js-mode
  :defer t
  :mode ("\\.js\\'" . js-mode)
  :mode ("\\.mjs\\'" . js-mode)
  :mode ("\\.cjs\\'" . js-mode)
  :mode ("\\.es\\'" . js-mode)
  :init
  (+javascript-common-config 'js-mode))

(use-package! js-ts-mode
  :defer t
  :when (modulep! +tree-sitter)
  :mode ("\\.js\\'" . js-ts-mode)
  :mode ("\\.mjs\\'" . js-ts-mode)
  :mode ("\\.cjs\\'" . js-ts-mode)
  :mode ("\\.es\\'" . js-ts-mode)
  :init
  (set-tree-sitter!
      'js-mode
      'js-ts-mode
    '((javascript :url "https://github.com/tree-sitter/tree-sitter-javascript"
       :rev "master"
       :source-dir "src")))
  (+javascript-common-config 'js-ts-mode '(javascript)))

(use-package! typescript-ts-mode
  :defer t
  :when (modulep! +tree-sitter)
  :mode ("\\.ts\\'" . typescript-ts-mode)
  :mode ("\\.mts\\'" . typescript-ts-mode)
  :mode ("\\.cts\\'" . typescript-ts-mode)
  :init
  (+javascript-common-config 'typescript-ts-mode '(typescript))
  (with-eval-after-load 'treesit
    (cl-pushnew '(typescript
                  "https://github.com/tree-sitter/tree-sitter-typescript"
                  "master"
                  "typescript/src"
                  nil
                  nil)
                treesit-language-source-alist :test #'eq :key #'car)))

(use-package! tsx-ts-mode
  :defer t
  :when (modulep! +tree-sitter)
  :mode ("\\.tsx\\'" . tsx-ts-mode)
  :mode ("\\.jsx\\'" . tsx-ts-mode)
  :init
  (+javascript-common-config 'tsx-ts-mode '(tsx typescript))
  (with-eval-after-load 'treesit
    (cl-pushnew '(tsx
                  "https://github.com/tree-sitter/tree-sitter-typescript"
                  "master"
                  "tsx/src"
                  nil
                  nil)
                treesit-language-source-alist :test #'eq :key #'car)))
