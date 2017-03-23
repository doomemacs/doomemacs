;; lang/javascript/config.el

(load! +screeps)

(def-package! js2-mode
  :mode "\\.js$"
  :interpreter "node"
  :config
  (setq js2-skip-preprocessor-directives t
        js2-highlight-external-variables nil
        js2-mode-show-parse-errors nil)

  (add-hook! 'js2-mode-hook
    '(flycheck-mode highlight-indent-guides-mode rainbow-delimiters-mode))

  ;; Conform switch-case indentation to editorconfig's config
  (add-hook! 'js2-mode-hook (setq js-switch-indent-offset js-indent-level))

  (set! :repl 'js2-mode 'nodejs-repl)
  (set! :electric 'js2-mode :chars '(?\} ?\) ?.) :words '("||" "&&"))
  (set! :xref-backend 'js2-mode 'xref-js2-xref-backend)

  (map! :map js2-mode-map
        :localleader
        :nv ";" 'doom/append-semicolon

        :prefix "r"
        :n  "g"  'js2r-add-to-globals-annotation
        :n  "ca" 'js2r-arguments-to-object
        :n  "Xa" 'js2r-contract-array
        :n  "Xf" 'js2r-contract-function
        :n  "Xo" 'js2r-contract-object
        :nv "d"  'js2r-debug-this
        :n  "xa" 'js2r-expand-array
        :n  "xf" 'js2r-expand-function
        :n  "xo" 'js2r-expand-object
        :v  "ef" 'js2r-extract-function
        :v  "em" 'js2r-extract-method
        :v  "ev" 'js2r-extract-var
        :n  "F"  'js2r-forward-barf
        :n  "f"  'js2r-forward-slurp
        :v  "ii" 'js2r-inject-global-in-iife
        :v  "iv" 'js2r-inline-var
        :v  "p"  'js2r-introduce-parameter
        :n  "p"  'js2r-localize-parameter
        :nv "l"  'js2r-log-this
        :n  "r"  'js2r-rename-var
        :n  "ss" 'js2r-split-string
        :n  "sv" 'js2r-split-var-declaration
        :n  "ct" 'js2r-ternary-to-if
        :v  "u"  'js2r-unwrap
        :n  "t"  'js2r-var-to-this
        :n  "ii" 'js2r-wrap-buffer-in-iife))


;; A find-{definition,references} backend for js2-mode. NOTE The xref API is
;; unstable and may break with an Emacs update.
(def-package! xref-js2 :commands xref-js2-xref-backend)


(def-package! nodejs-repl :commands nodejs-repl)


(def-package! js2-refactor
  :commands
  (js2r-extract-function js2r-extract-method js2r-introduce-parameter
   js2r-localize-parameter js2r-expand-object js2r-contract-object
   js2r-expand-function js2r-contract-function js2r-expand-array
   js2r-contract-array js2r-wrap-buffer-in-iife js2r-inject-global-in-iife
   js2r-add-to-globals-annotation js2r-extract-var js2r-inline-var
   js2r-rename-var js2r-var-to-this js2r-arguments-to-object js2r-ternary-to-if
   js2r-split-var-declaration js2r-split-string js2r-unwrap js2r-log-this
   js2r-debug-this js2r-forward-slurp js2r-forward-barf))


(def-package! tern
  :commands tern-mode
  :init (add-hook 'js2-mode-hook 'tern-mode))


(def-package! company-tern
  :when (featurep! :completion company)
  :after tern
  :config
  (set! :company-backend 'js2-mode '(company-tern)))


(def-package! jsx-mode :mode "\\.jsx$")


(def-package! coffee-mode
  :mode "\\.coffee$"
  :init (setq coffee-indent-like-python-mode t))


;;
;; Projects
;;

(def-project-mode! +javascript-gulp-mode
  :files "gulpfile.js")

(def-project-mode! +javascript-npm-mode
  :modes (web-mode js-mode markdown-mode)
  :files "package.json")

(def-project-mode! +javascript-lb6-mode
  :modes (web-mode js-mode nxml-mode markdown-mode)
  :match "\\.lb\\(action\\|ext\\)/"
  :init
  ;; TODO
  ;; (when IS-MAC
  ;;   (set! :build 'launchbar-action '+javascript-lb6-mode
  ;;     (lambda ()
  ;;       (when-let (dir (f-traverse-upwards (lambda (f) (f-ext? f "lbaction"))))
  ;;         (shell-command (format "open '%s'" dir))))))
  )

