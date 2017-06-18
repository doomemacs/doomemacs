;;; lang/typescript/config.el -*- lexical-binding: t; -*-

(def-package! typescript-mode
  :mode "\\.ts$"
  :init
  (add-hook 'typescript-mode-hook #'rainbow-delimiters-mode)
  :config
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

  (map! :localleader
        :m "fh" #'tide-documentation-at-point))


(def-package! tide
  :after typescript-mode
  :config
  (set! :company-backend 'typescript-mode '(company-tide))
  (set! :jump 'typescript-mode
    :definition #'tide-jump-to-definition
    :references #'tide-references
    :documentation #'tide-documentation-at-point)

  (setq tide-format-options
        '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t
          :placeOpenBraceOnNewLineForFunctions nil))

  (defun +typescript|init-tide ()
    (when (or (eq major-mode 'typescript-mode)
              (and (eq major-mode 'web-mode)
                   buffer-file-name
                   (equal (file-name-extension buffer-file-name) "tsx")))
      (tide-setup)
      (flycheck-mode +1)
      (eldoc-mode +1)))
  (add-hook! (typescript-mode web-mode) #'+typescript|init-tide)

  (advice-add #'tide-project-root :override #'doom*project-root))

