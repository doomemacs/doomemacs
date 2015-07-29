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

  (after! emr (require 'js2-refactor))

  ;; [pedantry intensifies]
  (defadvice js2-mode (after js2-mode-rename-modeline activate)
    (setq mode-name "JS2")))

(use-package tern
  :diminish tern-mode
  :commands tern-mode
  :init (add-hook! js2-mode 'tern-mode)
  :config
  (after! company
    (require 'company-tern)
    (add-company-backend! js2-mode (tern))))

(use-package unityjs-mode
  :mode "/Assets/.*\\.js$"
  :config
  (add-hook! unityjs-mode
    (flycheck-mode 1)
    (narf|enable-tab-width-2)
    (setq js-indent-level 2)))

(provide 'module-js)
;;; module-js.el ends here
