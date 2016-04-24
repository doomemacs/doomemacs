;;; module-js.el

(use-package js2-mode
  :mode "\\.js$"
  :interpreter "node"
  :init
  (def-repl! js2-mode nodejs-repl)
  (def-docset! js2-mode "js,javascript,nodejs,angularjs,express,jquery,mongoose")
  (def-company-backend! js2-mode (tern))
  (def-electric! js2-mode :chars (?\} ?\) ?.) :words ("||" "&&"))
  (add-hook! js2-mode '(tern-mode emr-initialize))

  :config
  (setq-default
   js2-skip-preprocessor-directives t
   js2-show-parse-errors nil
   js2-global-externs '("module" "require" "buster" "sinon" "assert"
                        "refute" "setTimeout" "clearTimeout"
                        "setInterval" "clearInterval" "location"
                        "__dirname" "console" "JSON" "jQuery" "$"
                        ;; Launchbar API
                        "LaunchBar" "File" "Action" "HTTP" "include" "Lib"))

  ;; [pedantry intensifies]
  (add-hook! js2-mode (setq mode-name "JS2"))

  (map! :map js2-mode-map (:localleader :nv ";" 'narf/append-semicolon)))

(use-package tern
  :after js2-mode
  :commands (tern-mode))

(use-package company-tern
  :after tern)

(use-package js2-refactor
  :after js2-mode
  :config
  (mapc (lambda (x)
          (let ((command-name (car x))
                (title (cadr x))
                (region-p (caddr x))
                predicate)
            (setq predicate (cond ((eq region-p 'both) nil)
                                  (t (if region-p
                                         (lambda () (use-region-p))
                                       (lambda () (not (use-region-p)))))))
            (emr-declare-command
                (intern (format "js2r-%s" (symbol-name command-name)))
              :title title :modes 'js2-mode :predicate predicate)))
        '((extract-function           "extract function"           t)
          (extract-method             "extract method"             t)
          (introduce-parameter        "introduce parameter"        t)
          (localize-parameter         "localize parameter"         nil)
          (expand-object              "expand object"              nil)
          (contract-object            "contract object"            nil)
          (expand-function            "expand function"            nil)
          (contract-function          "contract function"          nil)
          (expand-array               "expand array"               nil)
          (contract-array             "contract array"             nil)
          (wrap-buffer-in-iife        "wrap buffer in ii function" nil)
          (inject-global-in-iife      "inject global in ii function" t)
          (add-to-globals-annotation  "add to globals annotation"  nil)
          (extract-var                "extract variable"           t)
          (inline-var                 "inline variable"            t)
          (rename-var                 "rename variable"            nil)
          (var-to-this                "var to this"                nil)
          (arguments-to-object        "arguments to object"        nil)
          (ternary-to-if              "ternary to if"              nil)
          (split-var-declaration      "split var declaration"      nil)
          (split-string               "split string"               nil)
          (unwrap                     "unwrap"                     t)
          (log-this                   "log this"                   'both)
          (debug-this                 "debug this"                 'both)
          (forward-slurp              "forward slurp"              nil)
          (forward-barf               "forward barf"               nil))))

(use-package nodejs-repl :commands (nodejs-repl))

;;
(use-package jsx-mode :mode "\\.jsx$")

(use-package unityjs-mode
  :mode "/Assets/.*\\.js$"
  :config (add-hook 'unityjs-mode-hook 'flycheck-mode))

(use-package coffee-mode
  :mode "\\.coffee$"
  :config (setq-default coffee-indent-like-python-mode t))

;;
(def-project-type! nodejs "node"
  :modes (web-mode js-mode js2-mode json-mode coffee-mode scss-mode sass-mode less-css-mode)
  :files ("package.json"))

(def-project-type! angularjs "angular"
  :modes (web-mode js-mode js2-mode json-mode coffee-mode scss-mode sass-mode less-css-mode)
  :files ("public/libraries/angular/"))

(def-project-type! electron "electron"
  :modes (nodejs-project-mode)
  :files ("app/index.html" "app/main.js"))
;; TODO electron-compile support

(def-project-type! expressjs "express"
  :modes (nodejs-project-mode)
  :files ("node_modules/express/"))

;; TODO react

(provide 'module-js)
;;; module-js.el ends here
