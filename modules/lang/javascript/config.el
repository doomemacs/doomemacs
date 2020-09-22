;;; lang/javascript/config.el -*- lexical-binding: t; -*-

(after! (:any js2-mode rjsx-mode web-mode)
  (set-docsets! '(js2-mode rjsx-mode) "JavaScript"
    "AngularJS" "Backbone" "BackboneJS" "Bootstrap" "D3JS" "EmberJS" "Express"
    "ExtJS" "JQuery" "JQuery_Mobile" "JQuery_UI" "KnockoutJS" "Lo-Dash"
    "MarionetteJS" "MomentJS" "NodeJS" "PrototypeJS" "React" "RequireJS"
    "SailsJS" "UnderscoreJS" "VueJS" "ZeptoJS")

  (set-ligatures! '(js2-mode rjsx-mode web-mode)
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

(use-package! js2-mode
  :mode "\\.m?js\\'"
  :interpreter "node"
  :commands js2-line-break
  :config
  (setq js-chain-indent t
        ;; Don't mishighlight shebang lines
        js2-skip-preprocessor-directives t
        ;; let flycheck handle this
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        ;; Flycheck provides these features, so disable them: conflicting with
        ;; the eslint settings.
        js2-strict-trailing-comma-warning nil
        js2-strict-missing-semi-warning nil
        ;; maximum fontification
        js2-highlight-level 3
        js2-highlight-external-variables t
        js2-idle-timer-delay 0.1)

  (add-hook 'js2-mode-hook #'rainbow-delimiters-mode)
  ;; Indent switch-case another step
  (setq-hook! 'js2-mode-hook
    js-switch-indent-offset js2-basic-offset
    mode-name "JS2")

  (set-electric! 'js2-mode :chars '(?\} ?\) ?. ?:))
  (set-repl-handler! 'js2-mode #'+javascript/open-repl))


(use-package! rjsx-mode
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
  (when (featurep! :checkers syntax)
    (add-hook! 'rjsx-mode-hook
      ;; jshint doesn't know how to deal with jsx
      (push 'javascript-jshint flycheck-disabled-checkers)))

  ;; HACK `rjsx-electric-gt' relies on js2's parser to tell it when the cursor
  ;;      is in a self-closing tag, so that it can insert a matching ending tag
  ;;      at point. The parser doesn't run immediately however, so a fast typist
  ;;      can outrun it, causing tags to stay unclosed, so force it to parse:
  (defadvice! +javascript-reparse-a (n)
    ;; if n != 1, rjsx-electric-gt calls rjsx-maybe-reparse itself
    :before #'rjsx-electric-gt
    (if (= n 1) (rjsx-maybe-reparse))))


(use-package! typescript-mode
  :hook (typescript-mode . rainbow-delimiters-mode)
  :config
  (set-electric! 'typescript-mode
    :chars '(?\} ?\)) :words '("||" "&&"))
  (set-ligatures! 'typescript-mode
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
    :return "return" :yield "import")
  ;; HACK Fixes comment continuation on newline
  (setq-hook! 'typescript-mode-hook
    comment-line-break-function #'js2-line-break))

;; REVIEW We associate TSX files with `typescript-tsx-mode' derived from
;;        `web-mode' because `typescript-mode' does not officially support
;;        JSX/TSX. See
;;        https://github.com/emacs-typescript/typescript.el/issues/4
(if (featurep! :lang web)
    (progn
      (define-derived-mode typescript-tsx-mode web-mode "TypeScript-tsx")
      (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))

      (add-hook 'typescript-tsx-mode-hook #'emmet-mode)

      (after! flycheck
        (flycheck-add-mode 'typescript-tslint 'typescript-tsx-mode)
        (flycheck-add-mode 'javascript-eslint 'typescript-tsx-mode)))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode)))

(after! (:any typescript-mode web-mode)
  (set-docsets! '(typescript-mode typescript-tsx-mode) "TypeScript" "AngularTS"))


;;;###package coffee-mode
(setq coffee-indent-like-python-mode t)
(after! coffee-mode
  (set-docsets! 'coffee-mode "CoffeeScript"))


;;
;;; Tools

(add-hook! '(js2-mode-local-vars-hook
             typescript-mode-local-vars-hook
             typescript-tsx-mode-local-vars-hook
             web-mode-local-vars-hook
             rjsx-mode-local-vars-hook)
  (defun +javascript-init-lsp-or-tide-maybe-h ()
    "Start `lsp' or `tide' in the current buffer.

LSP will be used if the +lsp flag is enabled for :lang javascript AND if the
current buffer represents a file in a project.

If LSP fails to start (e.g. no available server or project), then we fall back
to tide."
    (let ((buffer-file-name (buffer-file-name (buffer-base-buffer))))
      (when (derived-mode-p 'js-mode 'typescript-mode 'typescript-tsx-mode)
        (if (not buffer-file-name)
            ;; necessary because `tide-setup' and `lsp' will error if not a
            ;; file-visiting buffer
            (add-hook 'after-save-hook #'+javascript-init-lsp-or-tide-maybe-h nil 'local)
          (or (and (featurep! +lsp) (lsp!))
              ;; fall back to tide
              (if (executable-find "node")
                  (and (require 'tide nil t)
                       (progn (tide-setup) tide-mode))
                (ignore
                 (doom-log "Couldn't start tide because 'node' is missing"))))
          (remove-hook 'after-save-hook #'+javascript-init-lsp-or-tide-maybe-h 'local))))))


(use-package! tide
  :defer t
  :config
  (setq tide-completion-detailed t
        tide-always-show-documentation t
        ;; Fix #1792: by default, tide ignores payloads larger than 100kb. This
        ;; is too small for larger projects that produce long completion lists,
        ;; so we up it to 512kb.
        tide-server-max-response-length 524288)
  ;; code completion
  (after! company
    ;; tide affects the global `company-backends', undo this so doom can handle
    ;; it buffer-locally
    (setq-default company-backends (delq 'company-tide (default-value 'company-backends))))
  (set-company-backend! 'tide-mode 'company-tide)
  ;; navigation
  (set-lookup-handlers! 'tide-mode :async t
    :xref-backend #'xref-tide-xref-backend
    :documentation #'tide-documentation-at-point)
  (set-popup-rule! "^\\*tide-documentation" :quit t)
  ;; resolve to `doom-project-root' if `tide-project-root' fails
  (advice-add #'tide-project-root :override #'+javascript-tide-project-root-a)
  ;; cleanup tsserver when no tide buffers are left
  (add-hook! 'tide-mode-hook
    (add-hook 'kill-buffer-hook #'+javascript-cleanup-tide-processes-h nil t))

  ;; Eldoc is activated too soon and disables itself, thinking there is no eldoc
  ;; support in the current buffer, so we must re-enable it later once eldoc
  ;; support exists. It is set *after* tide-mode is enabled, so enabling it on
  ;; `tide-mode-hook' is too early, so...
  (advice-add #'tide-setup :after #'eldoc-mode)

  (map! :localleader
        :map tide-mode-map
        "R"   #'tide-restart-server
        "f"   #'tide-format
        "rrs" #'tide-rename-symbol
        "roi" #'tide-organize-imports))


(use-package! xref-js2
  :when (featurep! :tools lookup)
  :after (:or js2-mode rjsx-mode)
  :config
  (set-lookup-handlers! '(js2-mode rjsx-mode)
    :xref-backend #'xref-js2-xref-backend))


(use-package! js2-refactor
  :hook ((js2-mode rjsx-mode) . js2-refactor-mode)
  :init
  (map! :after js2-mode
        :map js2-mode-map
        :localleader
        (:prefix ("r" . "refactor")
          (:prefix ("a" . "add/arguments"))
          (:prefix ("b" . "barf"))
          (:prefix ("c" . "contract"))
          (:prefix ("d" . "debug"))
          (:prefix ("e" . "expand/extract"))
          (:prefix ("i" . "inject/inline/introduce"))
          (:prefix ("l" . "localize/log"))
          (:prefix ("o" . "organize"))
          (:prefix ("r" . "rename"))
          (:prefix ("s" . "slurp/split/string"))
          (:prefix ("t" . "toggle"))
          (:prefix ("u" . "unwrap"))
          (:prefix ("v" . "var"))
          (:prefix ("w" . "wrap"))
          (:prefix ("3" . "ternary"))))
  :config
  (when (featurep! :editor evil +everywhere)
    (add-hook 'js2-refactor-mode-hook #'evil-normalize-keymaps)
    (let ((js2-refactor-mode-map (evil-get-auxiliary-keymap js2-refactor-mode-map 'normal t t)))
      (js2r-add-keybindings-with-prefix (format "%s r" doom-localleader-key)))))


;;;###package skewer-mode
(map! :localleader
      (:after js2-mode
        :map js2-mode-map
        "S" #'+javascript/skewer-this-buffer
        :prefix ("s" . "skewer"))
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
(use-package! npm-mode
  :hook ((js-mode typescript-mode) . npm-mode)
  :config
  (map! :localleader
        (:map npm-mode-keymap
          "n" npm-mode-command-keymap)
        (:after js2-mode
          :map js2-mode-map
          :prefix ("n" . "npm"))))


;;
;;; Projects

(def-project-mode! +javascript-npm-mode
  :modes '(html-mode
           css-mode
           web-mode
           markdown-mode
           js-mode
           json-mode
           typescript-mode
           typescript-tsx-mode
           solidity-mode)
  :when (locate-dominating-file default-directory "package.json")
  :add-hooks '(add-node-modules-path npm-mode))

(def-project-mode! +javascript-gulp-mode
  :when (locate-dominating-file default-directory "gulpfile.js"))
