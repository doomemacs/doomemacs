;;; lang/fsharp/config.el -*- lexical-binding: t; -*-

(defun +fsharp-common-config (mode)
  (set-formatter! 'fantomas '("fantomas" "--stdin") :modes (list mode))
  (when (modulep! +lsp)
    (add-hook (intern (format "%s-local-vars-hook" mode)) #'lsp! 'append)))


;;
;;; Packages

(use-package! fsharp-mode
  :defer t
  :config
  (+fsharp-common-config 'fsharp-mode)

  ;; REVIEW: Most of these can't be directly ported to fsharp-ts-mode, so...?
  (when (executable-find "dotnet")
    (setq inferior-fsharp-program "dotnet fsi --readline-"))
  (if (modulep! +lsp)
      (setq fsharp-ac-intellisense-enabled nil)
    (setq fsharp-ac-use-popup nil) ; Use a buffer for docs rather than a pop-up
    (set-lookup-handlers! 'fsharp-mode :async t :definition #'fsharp-ac/gotodefn-at-point)
    (set-company-backend! 'fsharp-mode 'fsharp-ac/company-backend))
  (set-repl-handler! 'fsharp-mode #'run-fsharp)
  (set-indent-vars! 'fsharp-mode '(fsharp-indent-offset fsharp-continuation-offset))
  (map! :localleader
        :map fsharp-mode-map
        "b" #'fsharp-ac/pop-gotodefn-stack ; Useful for re-tracing your steps
        "e" #'fsharp-eval-region
        "l" #'fsharp-load-buffer-file
        (:unless (modulep! +lsp)
         "q" #'fsharp-ac/stop-process
         "t" #'fsharp-ac/show-tooltip-at-point)))


(use-package! fsharp-ts-mode
  :when (modulep! +tree-sitter)
  :defer t
  :init
  (set-tree-sitter! 'fsharp-mode 'fsharp-ts-mode
    `((fsharp :url "https://github.com/ionide/tree-sitter-fsharp"
              :rev ,(if (< (treesit-library-abi-version) 15) "v0.1.0" "v0.2.0")
              :commit "594c500ecace8618db32dd1144307897277db067")))
  :config
  (+fsharp-common-config 'fsharp-ts-mode))
