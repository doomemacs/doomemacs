;;; lang/solidity/config.el -*- lexical-binding: t; -*-

;;
;;; Packages

(after! solidity-mode
  (setq solidity-comment-style 'slash)
  (set-docsets! 'solidity-mode "Solidity")
  (set-company-backend! 'solidity-mode 'company-solidity)
  (set-formatter! 'prettier-solidity '(npx "prettier" "--stdin-filepath" filepath "--parser=solidity") :modes '(solidity-mode))

  (use-package! solidity-flycheck  ; included with solidity-mode
    :when (and (modulep! :checkers syntax)
               (not (modulep! :checkers syntax +flymake)))
    :config
    (setq flycheck-solidity-solc-addstd-contracts t)
    (when (funcall flycheck-executable-find solidity-solc-path)
      (add-to-list 'flycheck-checkers 'solidity-checker nil #'eq))
    (when (funcall flycheck-executable-find solidity-solium-path)
      (add-to-list 'flycheck-checkers 'solium-checker nil #'eq)))

  (use-package! company-solidity
    :when (modulep! :completion company)
    :config (delq! 'company-solidity company-backends)))
