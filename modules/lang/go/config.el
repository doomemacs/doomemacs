;;; lang/go/config.el -*- lexical-binding: t; -*-

;;
;; Plugins
;;

(after! go-mode
  (set-env! "GOPATH" "GOROOT")
  (set-repl-handler! 'go-mode #'gorepl-run)
  (set-lookup-handlers! 'go-mode
    :definition #'go-guru-definition
    :references #'go-guru-referrers
    :documentation #'godoc-at-point)

  (when-let* ((goimports (executable-find "goimports")))
    (setq gofmt-command goimports))

  (when (featurep! :feature syntax-checker)
    (setq gofmt-show-errors nil)) ; Leave it to flycheck

  (add-hook 'go-mode-hook #'go-eldoc-setup)
  (add-hook! go-mode
    (add-hook 'before-save-hook #'gofmt-before-save nil t))

  (def-menu! +go/refactor-menu
    "Refactoring commands for `go-mode' buffers."
    '(("Add import"            :exec go-import-add            :region nil)
      ("Remove unused imports" :exec go-remove-unused-imports :region nil)
      ("Format buffer (gofmt)" :exec go-gofmt))
    :prompt "Refactor: ")

  (def-menu! +go/build-menu
    "Build/compilation commands for `go-mode' buffers."
    '(("Build project" :exec "go build")
      ("Build & run project" :exec "go run")
      ("Clean files" :exec "go clean"))
    :prompt "Run test: ")

  (def-menu! +go/test-menu
    "Test commands for `go-mode' buffers."
    '(("Last test"   :exec +go/test-rerun)
      ("All tests"   :exec +go/test-all)
      ("Single test" :exec +go/test-single)
      ("Nested test" :exec +go/test-nested))
    :prompt "Run test: ")

  (def-menu! +go/help-menu
    "Help and information commands for `go-mode' buffers."
    '(("Go to imports" :exec go-goto-imports)
      ("Lookup in godoc" :exec godoc-at-point)
      ("Describe this" :exec go-guru-describe)
      ("List free variables" :exec go-guru-freevars)
      ("What does this point to" :exec go-guru-pointsto)
      ("Implements relations for package types" :exec go-guru-implements :region nil)
      ("List peers for channel" :exec go-guru-peers)
      ("List references to object" :exec go-guru-referrers)
      ("Which errors" :exec go-guru-whicerrs)
      ("What query" :exec go-guru-what)
      ("Show callers of this function" :exec go-guru-callers :region nil)
      ("Show callees of this function" :exec go-guru-callees :region nil)))

  (map! :map go-mode-map
        :localleader
        :nr "r" #'+go/refactor-menu
        :n  "b" #'+go/build-menu
        :n  "h" #'+go/help-menu
        :n  "t" #'+go/test-menu
        :n  "r" #'go-play-buffer
        :v  "r" #'go-play-region))


(def-package! gorepl-mode
  :commands gorepl-run-load-current-file)


(def-package! company-go
  :when (featurep! :completion company)
  :after go-mode
  :config
  (set-company-backend! 'go-mode 'company-go)
  (setq company-go-show-annotation t))
