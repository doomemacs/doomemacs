;;; lang/lua/config.el -*- lexical-binding: t; -*-

;; sp's default rules are obnoxious, so disable them
(provide 'smartparens-lua)


;;
;;; Major modes

(defun +lua-common-config (mode)
  (set-lookup-handlers! mode :documentation 'lua-search-documentation)
  (set-electric! mode :words '("else" "end"))
  (set-repl-handler! mode #'+lua/open-repl)
  (set-company-backend! mode '(company-lua company-yasnippet))
  (when (modulep! +lsp)
    (add-hook (intern (format "%s-local-vars-hook" mode)) #'lsp! 'append)
    (when (modulep! :tools lsp +eglot)
      (set-eglot-client! mode (+lua-generate-lsp-server-command)))))


(use-package! lua-mode
  :interpreter "\\<lua\\(?:jit\\)?"
  :init
  (setq lua-indent-level 2)  ; default is 3; madness!
  :config
  (+lua-common-config 'lua-mode))


(use-package! lua-ts-mode
  :when (modulep! +tree-sitter)
  :defer t
  :init
  (set-tree-sitter! 'lua-mode 'lua-ts-mode
    `((lua :url "https://github.com/tree-sitter-grammars/tree-sitter-lua"
           :rev ,(if (< (treesit-library-abi-version) 15) "v0.3.0")
           :commit "db16e76558122e834ee214c8dc755b4a3edc82a9")))
  :config
  (+lua-common-config 'lua-ts-mode))


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
  :modes '(moonscript-mode lua-mode lua-ts-mode markdown-mode json-mode json-ts-mode)
  :when (+lua-love-project-root)
  :on-load
  (progn
    (set-project-type! 'love2d
      :predicate #'+lua-love-project-root
      :run #'+lua-love-build-command)
    (map! :localleader
          :map +lua-love-mode-map
          "b" #'+lua/run-love-game)))
