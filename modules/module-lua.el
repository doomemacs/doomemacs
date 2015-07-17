;;; module-lua.el --- lua + Love2D

(use-package lua-mode
  :mode "\\.lua$"
  :interpreter "lua"
  :init
  (setq-default lua-indent-level tab-width)

  ;; (after! company-dict
  ;;   (add-to-list 'company-dict-minor-mode-alist 'love-mode))

  (add-hook! lua-mode
    (narf|enable-tab-width-2)
    (setq lua-indent-level 2)))

(define-minor-mode love-mode
  "Buffer local minor mode for Love2D"
  :init-value nil
  :lighter " ♥"
  :keymap (make-sparse-keymap))
(add-yas-minor-mode! 'love-mode)
(associate! love-mode :files ("main.lua") :in (lua-mode))
(build-for! love-mode "open -a love.app '%s'" "main.lua")

(provide 'module-lua)
;;; module-lua.el ends here
