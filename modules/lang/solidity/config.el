;;; lang/solidity/config.el -*- lexical-binding: t; -*-

;;
;; Plugins
;;

(def-package! solidity-mode
  :mode "\\.sol$"
  :init
    (setq solidity-solc-path "$HOME/.node_modules/bin/solcjs")
    (setq solidity-solium-path "$HOME/.node_modules/bin/solium")

    (setq solidity-flycheck-solc-checker-active t)
    (setq solidity-flycheck-solium-checker-active t)

    (setq flycheck-solidity-solc-addstd-contracts t)
  :config
    (setq solidity-comment-style 'slash))

(def-package! company-solidity
  :when (featurep! :completion company)
  :after solidity-mode
  :config
  (add-hook 'solidity-mode-hook
    (lambda ()
    (set (make-local-variable 'company-backends)
        (append '((company-solidity company-capf company-dabbrev-code))
        company-backends)))))
