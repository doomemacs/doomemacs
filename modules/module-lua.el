;;; module-lua.el --- lua + Love2D

(use-package lua-mode
  :mode "\\.lua$"
  :interpreter "lua"
  :init (add-hook 'lua-mode-hook 'flycheck-mode)
  :config
  (def-company-backend! lua-mode (lua yasnippet))
  (def-electric! lua-mode :words ("else" "end"))
  (def-repl! lua-mode doom/inf-lua)
  (sp-with-modes '(lua-mode)
    ;; disable defaults
    (sp-local-pair "if" nil :actions       :rem)
    (sp-local-pair "while" nil :actions    :rem)
    (sp-local-pair "function" nil :actions :rem)
    (sp-local-pair "then " " end")
    (sp-local-pair "do "   " end")
    (sp-local-pair "then"  "end" :when '(("RET")) :post-handlers '("||\n[i]"))
    (sp-local-pair "do"    "end" :when '(("RET")) :post-handlers '("||\n[i]"))
    ;; block functions
    (sp-local-pair "function" "end" :when '(sp-point-after-bol-p) :post-handlers '(" |\n[i]"))
    ;; inline functions
    (sp-local-pair "function " " end" :unless '(sp-point-after-bol-p))))

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
