;;; lang/lua/config.el -*- lexical-binding: t; -*-

;; sp's default rules are obnoxious, so disable them
(provide 'smartparens-lua)


;;
;;; Major modes

(use-package! lua-mode
  :interpreter "\\<lua\\(?:jit\\)?"
  :init
  ;; lua-indent-level defaults to 3 otherwise. Madness.
  (setq lua-indent-level 2)

  :config
  (set-lookup-handlers! 'lua-mode :documentation 'lua-search-documentation)
  (set-electric! 'lua-mode :words '("else" "end"))
  (set-repl-handler! 'lua-mode #'+lua/open-repl)
  (set-company-backend! 'lua-mode '(company-lua company-yasnippet))

  (when (modulep! +lsp)
    (add-hook 'lua-mode-local-vars-hook #'lsp! 'append)
    (when (modulep! :tools lsp +eglot)
      (set-eglot-client! 'lua-mode (+lua-generate-lsp-server-command)))))


(use-package! lua-ts-mode
  :when (modulep! +tree-sitter)
  :when (fboundp 'lua-ts-mode) ; 30.1+ only
  :defer t
  :init
  (set-tree-sitter! 'lua-mode 'lua-ts-mode 'lua)
  :config
  (set-lookup-handlers! 'lua-ts-mode :documentation 'lua-search-documentation)
  (set-electric! 'lua-ts-mode :words '("else" "end"))
  (set-repl-handler! 'lua-ts-mode #'+lua/open-repl)
  (set-company-backend! 'lua-ts-mode '(company-lua company-yasnippet))

  (when (modulep! +lsp)
    (add-hook 'lua-ts-mode-local-vars-hook #'lsp! 'append)
    (when (modulep! :tools lsp +eglot)
      (set-eglot-client! 'lua-ts-mode (+lua-generate-lsp-server-command)))))


(use-package! moonscript
  :when (modulep! +moonscript)
  :defer t
  :config
  (setq-hook! 'moonscript-mode-hook
    moonscript-indent-offset tab-width)
  (add-hook! 'moonscript-mode-hook
             #'+lua-moonscript-fix-single-quotes-h
             #'+lua-moonscript-fontify-interpolation-h)
  (when (modulep! :checkers syntax -flymake)
    (require 'flycheck-moonscript nil t)))


;; TODO: fennel-ts-mode
(use-package! fennel-mode
  :when (modulep! +fennel)
  :mode "\\.fenneldoc\\'"
  :hook (fennel-mode . rainbow-delimiters-mode)
  :config
  (set-lookup-handlers! 'fennel-mode
    :definition #'fennel-find-definition
    :documentation #'fennel-show-documentation)
  (set-repl-handler! 'fennel-mode #'fennel-repl)

  (setq-hook! 'fennel-mode-hook
    ;; To match the `tab-width' default for other lisp modes
    tab-width 2
    ;; Don't treat autoloads or sexp openers as outline headers, we have
    ;; hideshow for that.
    outline-regexp "[ \t]*;;;;* [^ \t\n]"))


;;
;;; Frameworks

(def-project-mode! +lua-love-mode
  :modes '(moonscript-mode lua-mode lua-ts-mode markdown-mode json-mode)
  :when (+lua-love-project-root)
  :on-load
  (progn
    (set-project-type! 'love2d
      :predicate #'+lua-love-project-root
      :run #'+lua-love-build-command)
    (map! :localleader
          :map +lua-love-mode-map
          "b" #'+lua/run-love-game)))
