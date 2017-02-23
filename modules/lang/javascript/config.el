;; lang/javascript/config.el

;; TODO (load! +screeps)

(def-package! js2-mode
  :mode "\\.js$"
  :interpreter "node"
  :config
  (setq js2-skip-preprocessor-directives t
        js2-highlight-external-variables nil
        js2-mode-show-parse-errors nil)

  (add-hook! js2-mode '(tern-mode flycheck-mode highlight-indent-guides-mode))
  ;; Conform switch-case indentation to editorconfig's config
  (add-hook! js2-mode (setq js-switch-indent-offset js-indent-level))

  (set! :repl 'js2-mode 'nodejs-repl)

  (set! :company-backend 'js2-mode '(company-tern))

  (set! :electric 'js2-mode
        :chars ?\} ?\) ?.
        :words "||" "&&")

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


(def-package! company-tern
  :when (featurep 'company)
  :commands company-tern
  :after tern)


(def-package! jsx-mode :mode "\\.jsx$")


(def-package! coffee-mode
  :mode "\\.coffee$"
  :init (setq coffee-indent-like-python-mode t))


;;
;; Projects
;;

;; (project! npm
;;   :modes (web-mode js2-mode)
;;   :files ("package.json")
;;   :config
;;   (let* ((project-path (doom-project-root))
;;          (hash (gethash project-path npm-conf))
;;          (package-file (expand-file-name "package.json" project-path))
;;          deps)
;;     (when-let (json (and (not hash) (file-exists-p package-file)
;;                           (ignore-errors (json-read-file package-file))))
;;       (puthash project-path json npm-conf))))

