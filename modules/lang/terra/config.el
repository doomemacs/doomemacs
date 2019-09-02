;;; lang/lua/config.el -*- lexical-binding: t; -*-

;;
;; Major modes

(use-package! terra-mode
  :defer t
  :config
  (set-lookup-handlers! 'terra-mode :documentation 'terra-search-documentation)
  (set-electric! 'terra-mode :words '("else" "end"))
  (set-repl-handler! 'terra-mode #'+terra/open-repl)
  (set-company-backend! 'terra-mode '(company-lua company-yasnippet)))
