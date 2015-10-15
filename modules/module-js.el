;;; module-js.el

(use-package js2-mode
  :mode "\\.js$"
  :interpreter "node"
  :config
  (setq-default
   js2-skip-preprocessor-directives t
   js2-show-parse-errors nil
   js2-global-externs '("module" "require" "buster" "sinon" "assert"
                        "refute" "setTimeout" "clearTimeout"
                        "setInterval" "clearInterval" "location"
                        "__dirname" "console" "JSON" "jQuery" "$"
                        ;; Launchbar API
                        "LaunchBar" "File" "Action" "HTTP" "include"))

  (after! web-beautify
    (add-hook! js2-mode (setenv "jsbeautify_indent_size" "4"))
    (bind! :map js2-mode-map :m "gQ" 'web-beautify-js))

  (use-package js2-refactor
    :after emr
    :init (add-hook! js2-mode 'emr-initialize)
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
              (emr-declare-command (intern (format "js2r-%s" (symbol-name command-name)))
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

  ;; [pedantry intensifies]
  (defadvice js2-mode (after js2-mode-rename-modeline activate)
    (setq mode-name "JS2"))

  (define-minor-mode nodejs-mode
    :lighter " node" :keymap (make-sparse-keymap)
    (add-yas-minor-mode! 'nodejs-mode))
  (associate! nodejs-mode :files ("package.json") :in (js2-mode)))

(use-package tern
  :diminish tern-mode
  :commands tern-mode
  :init (add-hook! js2-mode 'tern-mode)
  :config
  (after! company
    (require 'company-tern)
    (define-company-backend! js2-mode (tern))))

(use-package unityjs-mode
  :mode "/Assets/.*\\.js$"
  :config
  (add-hook! unityjs-mode
    (flycheck-mode 1)
    (narf|enable-tab-width-2)
    (setq js-indent-level 2)))

(provide 'module-js)
;;; module-js.el ends here
