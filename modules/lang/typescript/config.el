;;; lang/typescript/config.el

(def-package! typescript-mode
  :mode "\\.ts$"
  :init
  (add-hook 'typescript-mode-hook 'rainbow-delimiters-mode)
  :config
  (set! :company-backend 'typescript-mode '(company-tide))
  (set! :electric 'typescript-mode :chars '(?\} ?\)) :words '("||" "&&"))

  ;; TODO tide-jump-back
  ;; TODO (tide-jump-to-definition t)
  ;; TODO convert into keybinds
  ;; (set! :emr 'typescript-mode
  ;;       '(tide-find-references             "find usages")
  ;;       '(tide-rename-symbol               "rename symbol")
  ;;       '(tide-jump-to-definition          "jump to definition")
  ;;       '(tide-documentation-at-point      "current type documentation")
  ;;       '(tide-restart-server              "restart tide server"))

  (defun +typescript|tide-setup ()
    (when (or (eq major-mode 'typescript-mode)
              (and (eq major-mode 'web-mode)
                   buffer-file-name
                   (string= (file-name-extension buffer-file-name) "tsx")))
      (tide-setup)
      (flycheck-mode +1)
      (eldoc-mode +1)))
  (add-hook! (typescript-mode web-mode) '+typescript|tide-setup)

  (advice-add 'tide-project-root :override 'doom-project-root)

  (map! :map typescript-mode-map
        :m "gd" 'tide-jump-to-definition
        :localleader
        :m "fh" 'tide-documentation-at-point))


(def-package! tide
  :after typescript-mode
  :config
  (setq tide-format-options
        '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t
          :placeOpenBraceOnNewLineForFunctions nil)))

