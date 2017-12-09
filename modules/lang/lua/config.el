;;; lang/lua/config.el --- lua + Love2D -*- lexical-binding: t; -*-

(def-package! lua-mode
  :mode "\\.lua$"
  :interpreter "lua"
  :config
  (add-hook 'lua-mode-hook #'flycheck-mode)

  (set! :electric 'lua-mode :words '("else" "end"))
  (set! :repl 'lua-mode #'+lua/repl)
  ;; sp's lua-specific rules are obnoxious, so we disable them
  (setq sp-pairs (delete (assq 'lua-mode sp-pairs) sp-pairs))

  (def-menu! +lua/build-menu
    "Build/compilation commands for `lua-mode' buffers."
    '(("Run Love app" :exec +lua/run-love-game :when +lua-love-mode))
    :prompt "Build tasks: ")

  (map! :map lua-mode-map
        :localleader
        "b" #'+lua/build-menu))


(def-package! company-lua
  :after (:all company lua-mode)
  :config
  (set! :company-backend 'lua-mode '(company-lua company-yasnippet)))


(def-package! moonscript
  :mode ("\\.moon$" . moonscript-mode)
  :config (defvaralias 'moonscript-indent-offset 'tab-width))


;;
;; Frameworks
;;

(def-project-mode! +lua-love-mode
  :modes (lua-mode markdown-mode json-mode)
  :files (and "main.lua" "conf.lua"))

