;;; lang/go/config.el -*- lexical-binding: t; -*-

;;
;;; Packages

(after! go-mode
  (set-docsets! 'go-mode "Go")
  (set-repl-handler! 'go-mode #'gorepl-run)
  (set-lookup-handlers! 'go-mode
    :documentation #'godoc-at-point)

  (when (modulep! +lsp)
    (add-hook 'go-mode-local-vars-hook #'lsp! 'append))

  (when (modulep! +tree-sitter)
    (add-hook 'go-mode-local-vars-hook #'tree-sitter! 'append))

  (map! :map go-mode-map
        :localleader
        "a" #'go-tag-add
        "d" #'go-tag-remove
        "e" #'+go/play-buffer-or-region
        "i" #'go-goto-imports      ; Go to imports
        (:prefix ("h" . "help")
          "." #'godoc-at-point)    ; Lookup in godoc
        (:prefix ("ri" . "imports")
          "a" #'go-import-add)
        (:prefix ("b" . "build")
          :desc "go run ." "r" (cmd! (compile "go run ."))
          :desc "go build" "b" (cmd! (compile "go build"))
          :desc "go clean" "c" (cmd! (compile "go clean")))
        (:prefix ("g" . "generate")
          "f" #'+go/generate-file
          "d" #'+go/generate-dir
          "a" #'+go/generate-all)
        (:prefix ("t" . "test")
          "t" #'+go/test-rerun
          "a" #'+go/test-all
          "s" #'+go/test-single
          "n" #'+go/test-nested
          "f" #'+go/test-file
          "g" #'go-gen-test-dwim
          "G" #'go-gen-test-all
          "e" #'go-gen-test-exported
          (:prefix ("b" . "bench")
            "s" #'+go/bench-single
            "a" #'+go/bench-all))))


(use-package! gorepl-mode
  :commands gorepl-run-load-current-file)


(use-package! flycheck-golangci-lint
  :when (modulep! :checkers syntax -flymake)
  :hook (go-mode . flycheck-golangci-lint-setup))
