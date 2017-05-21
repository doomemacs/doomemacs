;;; lang/purescript/config.el

(def-package! purescript-mode
  :mode "\\.purs$"
  :config
  (add-hook! 'purescript-mode-hook #'flycheck-mode)
  (add-hook! 'purescript-mode-hook #'company-mode)
  (add-hook! 'purescript-mode-hook #'purescript-indentation-mode)
  (add-hook! 'purescript-mode-hook #'rainbow-delimiters-mode)
  (load "purescript-mode-autoloads" nil t)
)

;; (def-package! flycheck-purescript
;;   :after purescript-mode
;;   :config
;;   (add-hook! 'flycheck-mode-hook #'flycheck-purescript-setup)
;; )

(def-package! psc-ide
  :after purescript-mode
  :config
  (require 'psc-ide)
  (add-hook! 'purescript-mode-hook #'psc-ide-mode)
)

