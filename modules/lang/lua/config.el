;;; lang/lua/config.el -*- lexical-binding: t; -*-

;; sp's default rules are obnoxious, so disable them
(provide 'smartparens-lua)


;;
;; Major modes

(def-package! lua-mode
  :defer t
  :init
  ;; lua-indent-level defaults to 3 otherwise. Madness.
  (setq lua-indent-level tab-width)
  :config
  (set-lookup-handlers! 'lua-mode :documentation 'lua-search-documentation)
  (set-electric! 'lua-mode :words '("else" "end"))
  (set-repl-handler! 'lua-mode #'+lua/open-repl)
  (set-company-backend! 'lua-mode '(company-lua company-yasnippet)))


;;;###package moonscript
(setq-hook! 'moonscript-mode-hook moonscript-indent-offset tab-width)


;;
;;; Frameworks

(def-project-mode! +lua-love-mode
  :modes (lua-mode markdown-mode json-mode)
  :files (and "main.lua" "conf.lua")
  :on-load
  (map! :localleader
        :map +lua-love-mode-map
        "b" #'+lua/run-love-game))
