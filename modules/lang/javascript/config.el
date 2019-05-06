;;; lang/javascript/config.el -*- lexical-binding: t; -*-

(after! (:any js2-mode rjsx-mode web-mode)
  (set-docsets! '(js2-mode rjsx-mode) "JavaScript"
    "AngularJS" "Backbone" "BackboneJS" "Bootstrap" "D3JS" "EmberJS" "Express"
    "ExtJS" "JQuery" "JQuery_Mobile" "JQuery_UI" "KnockoutJS" "Lo-Dash"
    "MarionetteJS" "MomentJS" "NodeJS" "PrototypeJS" "React" "RequireJS"
    "SailsJS" "UnderscoreJS" "VueJS" "ZeptoJS")

  (set-pretty-symbols! '(js2-mode rjsx-mode web-mode)
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
    :yield "import"))

(after! projectile
  (pushnew! projectile-project-root-files "package.json")
  (pushnew! projectile-globally-ignored-directories "node_modules" "flow-typed"))


;;
;; Major modes

(def-package! js2-mode
  :mode "\\.m?js\\'"
  :interpreter "node"
  :commands js2-line-break
  :config
  (setq js2-skip-preprocessor-directives t
        js-chain-indent t
        ;; let flycheck handle this
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        ;; Flycheck provides these features, so disable them: conflicting with
        ;; the eslint settings.
        js2-strict-trailing-comma-warning nil
        js2-strict-missing-semi-warning nil
        ;; maximum fontification
        js2-highlight-level 3
        js2-highlight-external-variables t)

  (add-hook 'js2-mode-hook #'rainbow-delimiters-mode)
  ;; Indent switch-case another step
  (setq-hook! 'js2-mode-hook
    js-switch-indent-offset js2-basic-offset
    mode-name "JS2")

  (set-electric! 'js2-mode :chars '(?\} ?\) ?. ?:))
  (set-repl-handler! 'js2-mode #'+javascript/open-repl)

  (map! :map js2-mode-map
        :localleader
        "S" #'+javascript/skewer-this-buffer))


(def-package! rjsx-mode
  :mode "components/.+\\.js$"
  :init
  (defun +javascript-jsx-file-p ()
    "Detect React or preact imports early in the file."
    (and buffer-file-name
         (string= (file-name-extension buffer-file-name) "js")
         (re-search-forward "\\(^\\s-*import +React\\|\\( from \\|require(\\)[\"']p?react\\)"
                            magic-mode-regexp-match-limit t)
         (progn (goto-char (match-beginning 1))
                (not (sp-point-in-string-or-comment)))))
  (add-to-list 'magic-mode-alist '(+javascript-jsx-file-p . rjsx-mode))
  :config
  (set-electric! 'rjsx-mode :chars '(?\} ?\) ?. ?>))
  (when (featurep! :tools flycheck)
    (add-hook! 'rjsx-mode-hook
      ;; jshint doesn't know how to deal with jsx
      (push 'javascript-jshint flycheck-disabled-checkers)))

  ;; `rjsx-electric-gt' relies on js2's parser to tell it when the cursor is in
  ;; a self-closing tag, so that it can insert a matching ending tag at point.
  ;; However, the parser doesn't run immediately, so a fast typist can outrun
  ;; it, causing tags to stay unclosed, so we force it to parse.
  (defun +javascript|reparse (n)
    ;; if n != 1, rjsx-electric-gt calls rjsx-maybe-reparse itself
    (if (= n 1) (rjsx-maybe-reparse)))
  (advice-add #'rjsx-electric-gt :before #'+javascript|reparse))


(after! typescript-mode
  (add-hook 'typescript-mode-hook #'rainbow-delimiters-mode)
  (setq-hook! 'typescript-mode-hook
    comment-line-break-function #'js2-line-break)
  (set-electric! 'typescript-mode
    :chars '(?\} ?\)) :words '("||" "&&"))
  (set-docsets! 'typescript-mode "TypeScript" "AngularTS")
  (set-pretty-symbols! 'typescript-mode
    ;; Functional
    :def "function"
    :lambda "() =>"
    :composition "compose"
    ;; Types
    :null "null"
    :true "true" :false "false"
    :int "number"
    :str "string"
    :bool "boolean"
    ;; Flow
    :not "!"
    :and "&&" :or "||"
    :for "for"
    :return "return" :yield "import"))


;;;###package coffee-mode
(setq coffee-indent-like-python-mode t)
(after! coffee-mode
  (set-docsets! 'coffee-mode "CoffeeScript"))


;;
;;; Tools

(def-package! tide
  :defer t
  :init
  (defun +javascript|init-tide-maybe ()
    "Enable `tide-mode' if node is available, `lsp-mode' isn't enabled and this
buffer represents a real file."
    (unless (bound-and-true-p lsp-mode)
      (cond ((not buffer-file-name)
             ;; necessary because `tide-setup' will error if not a file-visiting buffer
             (add-hook 'after-save-hook #'+javascript|init-tide-maybe nil 'local))
            ((executable-find "node")
             (tide-setup)
             (remove-hook 'after-save-hook #'+javascript|init-tide-maybe 'local))
            ((message "Couldn't find `node', aborting tide server")))))
  (add-hook! (js2-mode typescript-mode) #'+javascript|init-tide-maybe)

  (defun +javascript|init-tide-in-web-mode ()
    "Enable `tide-mode' if in a *.tsx file (and `lsp-mode' isn't active)."
    (when (string= (file-name-extension (or buffer-file-name "")) "tsx")
      (+javascript|init-tide-maybe)))
  (add-hook 'web-mode-hook #'+javascript|init-tide-in-web-mode)
  :config
  (setq tide-completion-detailed t
        tide-always-show-documentation t)
  ;; code completion
  (after! company
    ;; tide affects the global `company-backends', undo this so doom can handle
    ;; it buffer-locally
    (setq-default company-backends (delq 'company-tide (default-value 'company-backends))))
  (set-company-backend! 'tide-mode 'company-tide)
  ;; navigation
  (set-lookup-handlers! 'tide-mode
    :definition '(tide-jump-to-definition :async t)
    :references '(tide-references :async t))
  ;; resolve to `doom-project-root' if `tide-project-root' fails
  (advice-add #'tide-project-root :override #'+javascript*tide-project-root)
  ;; cleanup tsserver when no tide buffers are left
  (add-hook! 'tide-mode-hook
    (add-hook 'kill-buffer-hook #'+javascript|cleanup-tide-processes nil t))

  (define-key tide-mode-map [remap +lookup/documentation] #'tide-documentation-at-point)

  (map! :localleader
        :map tide-mode-map
        "R"   #'tide-restart-server
        "f"   #'tide-format
        "rs"  #'tide-rename-symbol
        "roi" #'tide-organize-imports))


(when (featurep! +lsp)
  (add-hook! (js2-mode typescript-mode) #'lsp!))


(def-package! xref-js2
  :when (featurep! :tools lookup)
  :after (:or js2-mode rjsx-mode)
  :config
  (set-lookup-handlers! '(js2-mode rjsx-mode)
    :xref-backend #'xref-js2-xref-backend))


(def-package! js2-refactor
  :hook ((js2-mode rjsx-mode) . js2-refactor-mode)
  :config
  (when (featurep! :editor evil +everywhere)
    (let ((js2-refactor-mode-map (evil-get-auxiliary-keymap js2-refactor-mode-map 'normal t t)))
      (js2r-add-keybindings-with-prefix (format "%s r" doom-localleader-key)))))


(def-package! eslintd-fix
  :commands eslintd-fix
  :config
  (defun +javascript|set-flycheck-executable-to-eslint ()
    (setq flycheck-javascript-eslint-executable eslintd-fix-executable))
  (add-hook 'eslintd-fix-mode-hook #'+javascript|set-flycheck-executable-to-eslint))


;;;###package skewer-mode
(map! :localleader
      :prefix "s"
      (:after skewer-mode
        :map skewer-mode-map
        "E" #'skewer-eval-last-expression
        "e" #'skewer-eval-defun
        "f" #'skewer-load-buffer)

      (:after skewer-css
        :map skewer-css-mode-map
        "e" #'skewer-css-eval-current-declaration
        "r" #'skewer-css-eval-current-rule
        "b" #'skewer-css-eval-buffer
        "c" #'skewer-css-clear-all)

      (:after skewer-html
        :map skewer-html-mode-map
        "e" #'skewer-html-eval-tag))


;;;###package npm-mode
(map! :after npm-mode
      :localleader
      :map npm-mode-keymap
      :prefix "n"
      "n" #'npm-mode-npm-init
      "i" #'npm-mode-npm-install
      "s" #'npm-mode-npm-install-save
      "d" #'npm-mode-npm-install-save-dev
      "u" #'npm-mode-npm-uninstall
      "l" #'npm-mode-npm-list
      "r" #'npm-mode-npm-run
      "v" #'npm-mode-visit-project-file)


;;
;;; Projects

(def-project-mode! +javascript-npm-mode
  :modes (html-mode css-mode web-mode typescript-mode js2-mode rjsx-mode json-mode markdown-mode)
  :when (locate-dominating-file default-directory "package.json")
  :add-hooks (+javascript|add-node-modules-path npm-mode))

(def-project-mode! +javascript-gulp-mode
  :when (locate-dominating-file default-directory "gulpfile.js"))
