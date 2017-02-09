;; lang/javascript/config.el
(provide '+javascript)

(@use-package js2-mode
  :mode "\\.js$"
  :interpreter "node"
  :init
  (setq js2-skip-preprocessor-directives t
        js2-highlight-external-variables nil
        js2-mode-show-parse-errors nil)

  :config
  (@add-hook '(tern-mode flycheck-mode highlight-indent-guides-mode))
  ;; Only way to conform switch-case indentation to editorconfig's config
  (@add-hook (setq js-switch-indent-offset js-indent-level))

  (@map :map js2-mode-map :localleader :nv ";" 'doom/append-semicolon)

  (@set :repl nodejs-repl)
  (@set :company-backends tern)
  (@set :electric-chars ?\} ?\) ?.)
  (@set :electric-words "||" "&&")
  (@set :emr
    (:v  js2r-extract-function           "extract function")
    (:v  js2r-extract-method             "extract method")
    (:v  js2r-introduce-parameter        "introduce parameter")
    (:n  js2r-localize-parameter         "localize parameter")
    (:n  js2r-expand-object              "expand object")
    (:n  js2r-contract-object            "contract object")
    (:n  js2r-expand-function            "expand function")
    (:n  js2r-contract-function          "contract function")
    (:n  js2r-expand-array               "expand array")
    (:n  js2r-contract-array             "contract array")
    (:n  js2r-wrap-buffer-in-iife        "wrap buffer in ii function")
    (:v  js2r-inject-global-in-iife      "inject global in ii function")
    (:n  js2r-add-to-globals-annotation  "add to globals annotation")
    (:v  js2r-extract-var                "extract variable")
    (:v  js2r-inline-var                 "inline variable")
    (:n  js2r-rename-var                 "rename variable")
    (:n  js2r-var-to-this                "var to this")
    (:n  js2r-arguments-to-object        "arguments to object")
    (:n  js2r-ternary-to-if              "ternary to if")
    (:n  js2r-split-var-declaration      "split var declaration")
    (:n  js2r-split-string               "split string")
    (:v  js2r-unwrap                     "unwrap")
    (:nv js2r-log-this                   "log this")
    (:nv js2r-debug-this                 "debug this")
    (:n  js2r-forward-slurp              "forward slurp")
    (:n  js2r-forward-barf               "forward barf")))

(@use-package js2-refactor :after emr)

(@use-package company-tern :commands company-tern :after tern)

(@use-package jsx-mode :mode "\\.jsx$")

(@use-package coffee-mode
  :mode "\\.coffee$"
  :init (setq coffee-indent-like-python-mode t))


;;
;; Projects
;;

(project! npm
  :modes (web-mode js2-mode)
  :files ("package.json")
  :config
  (let* ((project-path (doom-project-root))
         (hash (gethash project-path npm-conf))
         (package-file (expand-file-name "package.json" project-path))
         deps)
    (-when-let (json (and (not hash) (f-exists? package-file)
                          (ignore-errors (json-read-file package-file))))
      (puthash project-path json npm-conf))))

