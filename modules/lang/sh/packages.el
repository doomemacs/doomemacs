;; -*- no-byte-compile: t; -*-
;;; lang/sh/packages.el

;; requires shellcheck
;; optional: zshdb bashdb

(when (featurep! :completion company)
  (package! company-shell))

(when (featurep! +lsp)
  (depends-on! :tools lsp)
  (after! sh-script
    (lsp-define-stdio-client lsp-sh
                             "sh"
                             #'projectile-project-root
                             '("bash-language-server" "start"))
    (add-hook 'sh-mode-hook #'lsp-sh-enable)))
