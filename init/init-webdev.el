(provide 'init-webdev)

(use-package rainbow-mode
  :defer t
  :init (add-hook 'scss-mode-hook 'rainbow-mode))

(use-package scss-mode
  :mode "\\.scss$"
  :config
  (progn
    (add-hook 'scss-mode-hook 'enable-tab-width-2)
    (add-hook 'scss-mode-hook 'ac-css-mode-setup)

    (setq-default css-indent-offset 2)
    (setq scss-compile-at-save nil)

    (bind '(normal visual) scss-mode-map "gQ" 'web-beautify-css)))

;;; HTML/Markup
(use-package haml-mode :mode "\\.haml$")

(use-package web-mode
  :mode (("\\.\\(p\\)?htm\\(l\\)?$" . web-mode)
         ("\\.tpl\\(\\.php\\)?$" . web-mode)
         ("\\.erb$" . web-mode)
         ("wp-content/themes/.+/.+\\.php$" . web-mode))
  :config
  (progn
    (add-hook 'web-mode-hook 'enable-tab-width-2)

    (setq web-mode-ac-sources-alist '(("css" . (ac-source-css-property)))
          web-mode-markup-indent-offset  2
          web-mode-code-indent-offset    2
          web-mode-css-indent-offset     2
          web-mode-style-padding         2
          web-mode-script-padding        2
          web-mode-block-padding         2)

    (bind web-mode-map (kbd "s-/") 'web-mode-comment-or-uncomment)
    (bind 'normal  web-mode-map
          "zf" 'web-mode-fold-or-unfold
          ",t" 'web-mode-element-rename)
    (bind '(normal visual) web-mode-map
          "gQ" 'web-beautify-html
          "]a" 'web-mode-attribute-next
          "]t" 'web-mode-tag-next
          "[t" 'web-mode-tag-previous
          "]T" 'web-mode-element-child
          "[T" 'web-mode-element-parent)))

(use-package emmet-mode
  :defer t
  :init
  (progn
    (add-hook 'scss-mode-hook   'emmet-mode)
    (add-hook 'web-mode-hook    'emmet-mode)
    (add-hook 'html-mode-hook   'emmet-mode)
    (add-hook 'haml-mode-hook   'emmet-mode)
    (add-hook 'nxml-mode-hook   'emmet-mode))
  :config
  (progn
    (setq emmet-move-cursor-between-quotes t)

    (bind 'insert emmet-mode-keymap
          (kbd "s-e") 'emmet-expand-yas
          (kbd "s-E") 'emmet-expand-line)))

(use-package php-mode
  :mode "\\.\\(php\\|inc\\)$"
  :config
  (progn
    (add-hook! 'php-mode-hook (setq my-run-code-interpreter "php"))
    (setq php-template-compatibility nil)))

(use-package js2-mode :mode "\\.js$"
  :config
  (progn
    (setq-default js2-show-parse-errors nil)
    (setq-default js2-global-externs '("module" "require" "buster" "sinon" "assert"
                                       "refute" "setTimeout" "clearTimeout"
                                       "setInterval" "clearInterval" "location"
                                       "__dirname" "console" "JSON" "jQuery" "$"
                                       ;; Launchbar API
                                       "LaunchBar" "File" "Action" "HTTP" "include"))

    (bind '(normal visual) js2-mode-map "gQ" 'web-beautify-js)

    (use-package js2-refactor
      ;; :config
      ;; TODO Set up keymaps
      )

    (use-package tern
      :commands tern-mode
      ;; replace auto-complete with tern-ac-complete only in js-mode
      :init (add-hook! 'js2-mode-hook
                       (tern-mode t)
                       (tern-ac-setup)
                       (setq my-run-code-interpreter "node"))
      :config
      (progn
        (after "auto-complete"
               (require 'tern-auto-complete)
               (setq tern-ac-on-dot nil)
               (bind 'insert js2-mode-map
                     [remap auto-complete] 'tern-ac-complete)
               (bind 'motion js2-mode-map
                     "gd" 'tern-find-definition))

        (after "company-tern"
               (add-to-list 'company-backends 'company-tern)
               ;; (setq company-tern-meta-as-single-line t)
               ;; (setq company-tern-property-marker "")
               ;; (setq company-tooltip-align-annotations t)
               )))))


(use-package web-beautify
  :commands (web-beautify-js web-beautify-css web-beautify-html)
  :config
  (progn
    (add-hook! 'scss-mode-hook (setenv "jsbeautify_indent_size" "2"))
    (add-hook! 'web-mode-hook  (setenv "jsbeautify_indent_size" "4"))
    (add-hook! 'js2-mode-hook  (setenv "jsbeautify_indent_size" "4"))))
