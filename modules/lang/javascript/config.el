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
  (setq-hook! 'js2-mode-hook js-switch-indent-offset js2-basic-offset)

  (set-electric! 'js2-mode :chars '(?\} ?\) ?. ?:))
  (set-repl-handler! 'js2-mode #'+javascript/repl)

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
  (when (featurep! :feature syntax-checker)
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


;; `coffee-mode'
(setq coffee-indent-like-python-mode t)
(after! coffee-mode
  (set-docsets! 'coffee-mode "CoffeeScript"))


;;
;; Tools

(def-package! tide
  :defer t
  :init
  ;; Don't let hard errors stop the user from opening js files.
  (defun +javascript|init-tide ()
    "Enable `tide-mode' if node is available."
    (if (executable-find "node")
        (tide-setup)
      (message "Couldn't find `node', aborting tide server")))
  (add-hook! (js2-mode typescript-mode) #'+javascript|init-tide)

  (defun +javascript|init-tide-in-web-mode ()
    "Enable `tide-mode' if in a *.tsx file."
    (when (string= (file-name-extension (or buffer-file-name "")) "tsx")
      (tide-setup)))
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
    :definition #'tide-jump-to-definition
    :references #'tide-references
    :documentation #'tide-documentation-at-point)
  ;; resolve to `doom-project-root' if `tide-project-root' fails
  (advice-add #'tide-project-root :override #'+javascript*tide-project-root)
  ;; cleanup tsserver when no tide buffers are left
  (add-hook! 'tide-mode-hook
    (add-hook 'kill-buffer-hook #'+javascript|cleanup-tide-processes nil t))

  (map! :localleader
        :map tide-mode-map
        "R"   #'tide-restart-server
        "f"   #'tide-reformat
        "rs"  #'tide-rename-symbol
        "roi" #'tide-organize-imports))


(def-package! xref-js2
  :when (featurep! :feature lookup)
  :after (:or js2-mode rjsx-mode)
  :config
  (set-lookup-handlers! '(js2-mode rjsx-mode)
    :xref-backend #'xref-js2-xref-backend))


(def-package! js2-refactor
  :hook ((js2-mode rjsx-mode) . js2-refactor-mode)
  :config
  (when (featurep! :feature evil +everywhere)
    (let ((js2-refactor-mode-map (evil-get-auxiliary-keymap js2-refactor-mode-map 'normal t t)))
      (js2r-add-keybindings-with-prefix (format "%s r" doom-localleader-key)))))


(def-package! eslintd-fix
  :commands eslintd-fix
  :config
  (defun +javascript|set-flycheck-executable-to-eslint ()
    (setq flycheck-javascript-eslint-executable eslintd-fix-executable))
  (add-hook 'eslintd-fix-mode-hook #'+javascript|set-flycheck-executable-to-eslint))


;; `skewer-mode'
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


;; `npm-mode'
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
;; Projects

(def-project-mode! +javascript-npm-mode
  :modes (html-mode css-mode web-mode typescript-mode js2-mode rjsx-mode json-mode markdown-mode)
  :when (locate-dominating-file default-directory "package.json")
  :add-hooks (+javascript|add-node-modules-path npm-mode))

(def-project-mode! +javascript-gulp-mode
  :when (locate-dominating-file default-directory "gulpfile.js"))

(def-project-mode! +javascript-screeps-mode
  :match "/screeps\\(?:-ai\\)?/.+$"
  :modes (+javascript-npm-mode)
  :add-hooks (+javascript|init-screeps-mode)
  :on-load (load! "+screeps"))
