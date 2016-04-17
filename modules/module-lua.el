;;; module-lua.el --- lua + Love2D

(use-package lua-mode
  :mode "\\.lua$"
  :interpreter "lua"
  :init
  (define-repl! lua-mode narf/inf-lua)
  (define-company-backend! lua-mode (yasnippet))
  (add-hook 'lua-mode-hook 'flycheck-mode)
  (add-hook! lua-mode
    (electric-indent-local-mode +1)
    (setq narf-electric-indent-words '("else" "end")))
  :config
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

;;
(define-project-type! love "♥"
  :modes (lua-mode markdown-mode json-mode)
  :files ("main.lua" "conf.lua")
  :build ("open -a love.app '%s'" "main.lua"))

(define-project-type! hammerspoon "hammer"
  :modes (lua-mode markdown-mode)
  :match "/\\.?hammerspoon/.+\\.lua$"
  :build "open hammerspoon://reload")

(provide 'module-lua)
;;; module-lua.el ends here
