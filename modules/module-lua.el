;;; module-lua.el --- lua + Love2D

(use-package lua-mode
  :mode "\\.lua$"
  :interpreter "lua"
  :init
  (define-repl! lua-mode narf/inf-lua)
  (define-company-backend! lua-mode (yasnippet))
  (add-hook 'lua-mode-hook 'flycheck-mode)
  (after! company-dict
    (add-to-list 'company-dict-minor-mode-list 'love-mode))
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

(define-minor-mode love-mode
  "Buffer local minor mode for Love2D"
  :init-value nil
  :lighter " ♥"
  :keymap (make-sparse-keymap)
  (add-yas-minor-mode! 'love-mode))
(associate! love-mode
  :in (lua-mode markdown-mode json-mode)
  :files ("main.lua" "conf.lua"))
(define-builder! love-mode "open -a love.app '%s'" "main.lua")

(define-minor-mode hammerspoon-mode
  :init-value nil
  :lighter " hammer"
  :keymap (make-sparse-keymap)
  (add-yas-minor-mode! 'hammerspoon-mode))
(associate! hammerspoon-mode
  :in (lua-mode markdown-mode)
  :match "/\\.?hammerspoon/.+\\.lua$")
(define-builder! hammerspoon-mode "open hammerspoon://reload")

(provide 'module-lua)
;;; module-lua.el ends here
