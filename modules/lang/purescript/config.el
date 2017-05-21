;;; lang/purescript/config.el

(def-package! purescript-mode
  :mode "\\.purs$"
  :init
  (add-hook 'purescript-mode-hook #'flycheck-mode)
  (add-hook 'purescript-mode-hook #'rainbow-delimiters-mode)
  :config
  (load "purescript-mode-autoloads" nil t)
)

;; Seems broken at the moment

;; (def-package! flycheck-purescript
;;   :after purescript-mode
;;   :config
;;   (add-hook! 'flycheck-mode-hook #'flycheck-purescript-setup)
;; )

(def-package! psc-ide
  :after purescript-mode
  :config
  (require 'psc-ide)
  (add-hook! 'purescript-mode-hook
    (lambda ()
      (company-mode)
      (psc-ide-mode)
      (turn-on-purescript-indentation)
    )
  )
)

