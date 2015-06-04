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
      (bind :motion :map js2-mode-map "gQ" 'web-beautify-js))

    (after "emr" (use-package js2-refactor))

    ;; [pedantry intensifies]
    (defadvice js2-mode (after js2-mode-rename-modeline activate)
      (setq mode-name "Javascript2"))

    (use-package tern
      :diminish (tern-mode . "tern")
      :commands tern-mode
      :init
      (add-hook 'js2-mode-hook 'tern-mode)
      :config
      (after "company"
        (use-package company-tern
          :config
          (narf/add-company-backend js2-mode (company-tern)))))))

;; For UnityScript
(use-package unityjs-mode
  :mode "/Assets/*.js$"
  :config
  (progn
    (add-hook 'unityjs-mode-hook 'flycheck-mode)
    (add-hook! 'unityjs-mode-hook
               (narf|enable-tab-width-2)
               (setq js-indent-level 2))))

;; For launchbar script development
(define-minor-mode lb6-mode
  "Launchbar development mode."
  :init-value nil
  :lighter    "lb6"
  :keymap     (make-sparse-keymap)
  (narf/init-yas-mode 'lb6-mode))
(associate-minor-mode "\\.lb\\(action\\|ext\\)/.*$" 'lb6-mode)


(provide 'init-js)
;;; init-js.el ends here
