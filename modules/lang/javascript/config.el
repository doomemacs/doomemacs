;;; lang/javascript/config.el -*- lexical-binding: t; -*-

(after! projectile
  (pushnew! projectile-project-root-files "package.json")
  (pushnew! projectile-globally-ignored-directories "node_modules" "flow-typed"))


;;
;;; Major modes

(defun +javascript-common-config (mode)
  (unless (eq mode 'nodejs-repl-mode)
    (set-repl-handler! mode #'+javascript/open-repl)
    (set-electric! mode :chars '(?\} ?\) ?. ?:))
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
      :yield "import")

    (when (modulep! +lsp)
      (add-hook (intern (format "%s-local-vars-hook" mode)) #'lsp! 'append)))

  (if (not (memq mode '(typescript-mode typescript-ts-mode)))
      (set-docsets! mode "JavaScript"
        "AngularJS" "Backbone" "BackboneJS" "Bootstrap" "D3JS" "EmberJS" "Express"
        "ExtJS" "JQuery" "JQuery_Mobile" "JQuery_UI" "KnockoutJS" "Lo-Dash"
        "MarionetteJS" "MomentJS" "NodeJS" "PrototypeJS" "React" "RequireJS"
        "SailsJS" "UnderscoreJS" "VueJS" "ZeptoJS")
    (set-docsets! mode :add "TypeScript" "AngularTS")
    (set-electric! mode :chars '(?\} ?\)) :words '("||" "&&"))))


(use-package! js
  :mode ("\\.[mc]?js\\'" . js-mode)
  :mode ("\\.es6\\'" . js-mode)
  :mode ("\\.pac\\'" . js-mode)
  :init
  (when (modulep! +tree-sitter)  ; 29.1+ only
    (set-tree-sitter! 'js-mode 'js-ts-mode
      `((javascript :url "https://github.com/tree-sitter/tree-sitter-javascript"
                    :rev ,(if (< (treesit-library-abi-version) 15) "v0.23.0" "v0.25.0"))
        (jsdoc :url "https://github.com/tree-sitter/tree-sitter-jsdoc"
               :rev "v0.23.2"))))
  :config
  (+javascript-common-config 'js-mode)
  (when (modulep! +tree-sitter)
    (+javascript-common-config 'js-ts-mode))

  (setq js-chain-indent t))


(use-package! typescript-mode
  :unless (modulep! +tree-sitter)
  :mode "\\.ts\\'"
  :config
  (+javascript-common-config 'typescript-mode))


(use-package! typescript-ts-mode  ; 29.1+ only
  :when (modulep! +tree-sitter)
  :mode "\\.ts\\'"
  :mode ("\\.[tj]sx\\'" . tsx-ts-mode)
  :init
  (set-tree-sitter! 'typescript-mode 'typescript-ts-mode
    '((typescript :url "https://github.com/tree-sitter/tree-sitter-typescript"
                  :commit "8e13e1db35b941fc57f2bd2dd4628180448c17d5"
                  :source-dir "typescript/src")))
  (set-tree-sitter! nil 'tsx-ts-mode
    '((tsx :url "https://github.com/tree-sitter/tree-sitter-typescript"
           :commit "8e13e1db35b941fc57f2bd2dd4628180448c17d5"
           :source-dir "tsx/src")))
  :config
  (+javascript-common-config 'typescript-ts-mode)
  (+javascript-common-config 'tsx-ts-mode))


;;
;;; Extensions

;; Parse node stack traces in the compilation buffer
(after! compilation
  (add-to-list 'compilation-error-regexp-alist 'node)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(node "^[[:blank:]]*at \\(.*(\\|\\)\\(.+?\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)"
                 2 3 4)))


(use-package! nodejs-repl
  :defer t
  :init
  :config
  (+javascript-common-config 'nodejs-repl-mode))


;;
;;; Projects

(def-project-mode! +javascript-npm-mode
  :modes '(html-mode
           css-mode
           web-mode
           markdown-mode
           js-mode  ; includes js2-mode and rjsx-mode
           js-ts-mode
           json-mode
           json-ts-mode
           typescript-mode
           typescript-ts-mode
           tsx-ts-mode
           solidity-mode)
  :when (locate-dominating-file default-directory "package.json")
  :add-hooks '(+javascript-add-npm-path-h))

(def-project-mode! +javascript-gulp-mode
  :when (locate-dominating-file default-directory "gulpfile.js"))
