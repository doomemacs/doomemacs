;;; lang/fsharp/config.el -*- lexical-binding: t; -*-

(after! fsharp-mode
  (if (featurep! +lsp)
      (progn
        (setq fsharp-ac-intellisense-enabled nil)
        (setq lsp-fsharp-server-install-dir (concat doom-etc-dir "fsautocomplete/"))
        (add-hook 'fsharp-mode-local-vars-hook #'lsp!))
    (setq fsharp-ac-use-popup nil) ; Use a buffer for docs rather than a pop-up
    (set-lookup-handlers! 'fsharp-mode :async t :definition #'fsharp-ac/gotodefn-at-point)
    (set-company-backend! 'fsharp-mode 'fsharp-ac/company-backend))
  (set-repl-handler! 'fsharp-mode #'run-fsharp)
  (map! :localleader
        :map fsharp-mode-map
        "b" #'fsharp-ac/pop-gotodefn-stack ; Useful for re-tracing your steps
        "e" #'fsharp-eval-region
        "l" #'fsharp-load-buffer-file
        (:unless (featurep! +lsp)
          "q" #'fsharp-ac/stop-process
          "t" #'fsharp-ac/show-tooltip-at-point)))
