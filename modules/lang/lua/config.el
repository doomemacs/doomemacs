;;; module-lua.el --- lua + Love2D

(use-package lua-mode
  :mode "\\.lua$"
  :interpreter "lua"
  :init (add-hook 'lua-mode-hook 'flycheck-mode)
  :config
  (def-company-backend! lua-mode (lua yasnippet))
  (def-electric! lua-mode :words ("else" "end"))
  (def-repl! lua-mode doom/inf-lua)

  ;; sp's lua-specific rules are obnoxious, so we disable them
  (setq sp-pairs (delete (assq 'lua-mode sp-pairs) sp-pairs)))

(use-package company-lua :after lua-mode)

(use-package moonscript
  :mode ("\\.moon$" . moonscript-mode)
  :config
  (push '(moonscript-mode moonscript-indent-offset) editorconfig-indentation-alist))

;;
(def-project-type! love "♥"
  :modes (lua-mode markdown-mode json-mode)
  :files ("main.lua" "conf.lua")
  :build ("open -a love.app '%s'" "main.lua"))

(def-project-type! hammerspoon "hammer"
  :modes (lua-mode markdown-mode)
  :match "/\\.?hammerspoon/.+\\.lua$"
  :build "open hammerspoon://reload")

(provide 'module-lua)
;;; module-lua.el ends here
