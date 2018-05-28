;;; lang/solidity/config.el -*- lexical-binding: t; -*-

;;
;; Plugins
;;

;; `solidity-mode'
(setq solidity-comment-style 'slash
      solidity-flycheck-solc-checker-active t
      solidity-flycheck-solium-checker-active t)


(def-package! solidity-flycheck  ; included with solidity-mode
  :when (featurep! :feature syntax-checker)
  :after solidity-mode
  :init (add-hook 'solidity-mode-hook #'flycheck-mode)
  :config (setq flycheck-solidity-solc-addstd-contracts t))
  

(def-package! company-solidity
  :when (featurep! :completion company)
  :after solidity-mode
  :config
  (setq company-backends (delq 'company-solidity company-backends))
  (set! :company-backends 'solidity-mode 'company-solidity))
