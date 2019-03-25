;;; lang/lua/config.el -*- lexical-binding: t; -*-

;; sp's default rules are obnoxious, so disable them
(provide 'smartparens-terra)


;;
;; Major modes

(def-package! terra-mode
  :defer t
  :init
  ;; lua-indent-level defaults to 3 otherwise. Madness.
  (setq terra-indent-level tab-width)
  :config
  (set-lookup-handlers! 'terra-mode :documentation 'terra-search-documentation)
  (set-electric! 'terra-mode :words '("else" "end"))
  (set-repl-handler! 'terra-mode #'+terra/open-repl)
  (set-company-backend! 'terra-mode '(company-lua company-yasnippet)))


;;
;; Frameworks

