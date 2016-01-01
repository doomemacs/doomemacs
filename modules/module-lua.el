;;; module-lua.el --- lua + Love2D

(use-package lua-mode
  :mode "\\.lua$"
  :interpreter "lua"
  :init
  (define-repl! lua-mode narf-inf-lua)
  (add-hook! lua-mode 'flycheck-mode)
  ;; (after! company-dict
  ;;   (add-to-list 'company-dict-minor-mode-alist 'love-mode))

  (defun narf-inf-lua ()
    (lua-start-process "lua" "lua")
    (pop-to-buffer lua-process-buffer)))

(define-minor-mode love-mode
  "Buffer local minor mode for Love2D"
  :init-value nil
  :lighter " ♥"
  :keymap (make-sparse-keymap)
  (add-yas-minor-mode! 'love-mode))
(associate! love-mode :files ("main.lua") :in (lua-mode))
(define-builder! love-mode "open -a love.app '%s'" "main.lua")

(provide 'module-lua)
;;; module-lua.el ends here
