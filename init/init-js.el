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
      (progn
        (after "auto-complete"
          (use-package tern-auto-complete
            :config
            (progn
              (setq tern-ac-on-dot nil)
              (bind 'insert js2-mode-map [remap auto-complete] 'tern-ac-complete)
              (bind 'motion js2-mode-map "gd" 'tern-find-definition))))

        (after "company"
          (use-package company-tern
            :config
            (company--backend-on 'js2-mode-hook 'company-tern)
            ;; (setq company-tern-meta-as-single-line t)
            ;; (setq company-tern-property-marker "")
            ;; (setq company-tooltip-align-annotations t)
            ))))))

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


(provide 'init-js)
;;; init-js.el ends here
