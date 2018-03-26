;;; lang/typescript/config.el -*- lexical-binding: t; -*-

(def-package! typescript-mode
  :mode "\\.ts$"
  :config
  (add-hook 'typescript-mode-hook #'rainbow-delimiters-mode)
  (set! :electric 'typescript-mode :chars '(?\} ?\)) :words '("||" "&&")))


(def-package! tide
  :hook (typescript-mode . tide-setup)
  :init
  (defun +typescript|init-tide-in-web-mode ()
    (when (string= (file-name-extension (or buffer-file-name "")) "tsx")
      (tide-setup)))
  (add-hook 'web-mode-hook #'+typescript|init-tide-in-web-mode)
  :config
  (set! :company-backend 'typescript-mode '(company-tide))
  (set! :lookup 'typescript-mode
    :definition #'tide-jump-to-definition
    :references #'tide-references
    :documentation #'tide-documentation-at-point)

  (setq tide-format-options
        '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t
          :placeOpenBraceOnNewLineForFunctions nil))

  (def-menu! +typescript/refactor-menu
    "TODO"
    '(("rename symbol"              :exec tide-rename-symbol)
      ("restart tide server"        :exec tide-restart-server)))

  (map! :map tide-mode-map
        :localleader
        :n "r" #'+typescript/refactor-menu)

  (add-hook! 'tide-mode-hook #'(flycheck-mode eldoc-mode)))

