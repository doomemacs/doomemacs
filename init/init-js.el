(use-package js2-mode
  :mode "\\.js$"
  :interpreter "node"
  :config
  (progn
    (setq js2-skip-preprocessor-directives t)
    (setq-default js2-show-parse-errors nil)
    (setq-default js2-global-externs '("module" "require" "buster" "sinon" "assert"
                                       "refute" "setTimeout" "clearTimeout"
                                       "setInterval" "clearInterval" "location"
                                       "__dirname" "console" "JSON" "jQuery" "$"
                                       ;; Launchbar API
                                       "LaunchBar" "File" "Action" "HTTP" "include"))

    (after "web-beautify"
      (add-hook! 'js2-mode-hook (setenv "jsbeautify_indent_size" "4"))
      (bind 'motion js2-mode-map "gQ" 'web-beautify-js))

    (after "emr" (use-package js2-refactor))

    (use-package tern
      :commands tern-mode
      :init
      (progn
        (add-hook 'js2-mode-hook 'tern-mode)
        (after "auto-complete" (add-hook 'js2-mode-hook 'tern-ac-setup)))
      :config
      (after "company"
        (use-package company-tern
          :config
          (company--backend-on 'js2-mode-hook 'company-tern)
          ;; (setq company-tern-meta-as-single-line t)
          ;; (setq company-tern-property-marker "")
          ;; (setq company-tooltip-align-annotations t)
          )))))

(use-package json-mode
  :mode (("\\.json$" . json-mode)
         ("\\.jshintrc$" . json-mode)))

;; For UnityScript
(use-package unityjs-mode
  :mode "/Assets/*.js$"
  :config
  (progn
    (add-hook 'unityjs-mode-hook 'flycheck-mode)
    (add-hook! 'unityjs-mode-hook
               (enable-tab-width-2)
               (setq js-indent-level 2))))

(define-minor-mode lb6-mode
  :lighter " lb6"
  :keymap (make-sparse-keymap)
  (my--init-yas-mode 'lb6-mode))
(associate-minor-mode "\\.lb\\(action\\|ext\\)/.*$" 'lb6-mode)

(provide 'init-js)
;;; init-js.el ends here
