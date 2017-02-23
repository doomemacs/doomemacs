;;; lang/lua/config.el --- lua + Love2D

(def-package! lua-mode
  :mode "\\.lua$"
  :interpreter "lua"
  :init
  (add-hook 'lua-mode-hook 'flycheck-mode)
  :config
  (set! :company-backend 'lua-mode '(company-lua company-yasnippet))
  (set! :electric 'lua-mode :words '("else" "end"))
  (set! :repl 'lua-mode '+lua/repl)

  ;; sp's lua-specific rules are obnoxious, so we disable them
  (setq sp-pairs (delete (assq 'lua-mode sp-pairs) sp-pairs)))


(def-package! company-lua
  :after lua-mode)


(def-package! moonscript
  :mode ("\\.moon$" . moonscript-mode)
  :config
  (push '(moonscript-mode moonscript-indent-offset) editorconfig-indentation-alist))


;;
;; TODO Frameworks
;;

;; (def-project-type! love "♥"
;;   :modes (lua-mode markdown-mode json-mode)
;;   :files ("main.lua" "conf.lua")
;;   :build ("open -a love.app '%s'" "main.lua"))

;; (def-project-type! hammerspoon "hammer"
;;   :modes (lua-mode markdown-mode)
;;   :match "/\\.?hammerspoon/.+\\.lua$"
;;   :build "open hammerspoon://reload")

