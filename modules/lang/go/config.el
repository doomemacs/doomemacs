;;; lang/go/config.el -*- lexical-binding: t; -*-

;;
;; Packages

(after! go-mode
  (set-env! "GOPATH" "GOROOT")
  (set-docsets! 'go-mode "Go")
  (set-repl-handler! 'go-mode #'gorepl-run)
  (set-lookup-handlers! 'go-mode
    :definition #'go-guru-definition
    :references #'go-guru-referrers
    :documentation #'godoc-at-point)

  ;; Redefines default formatter to *not* use goimports if reformatting a
  ;; region; as it doesn't play well with partial code.
  (set-formatter! 'gofmt
    '(("%s" (if (or +format-region-p
                    (not (executable-find "goimports")))
                "gofmt"
              "goimports"))))

  (add-hook 'go-mode-hook #'go-eldoc-setup)

  (map! :map go-mode-map
        :localleader
        "e" #'+go/play-buffer-or-region
        "i" #'go-goto-imports      ; Go to imports
        (:prefix ("h" . "help")
          "." #'godoc-at-point     ; Lookup in godoc
          "d" #'go-guru-describe   ; Describe this
          "v" #'go-guru-freevars   ; List free variables
          "i" #'go-guru-implements ; Implements relations for package types
          "p" #'go-guru-peers      ; List peers for channel
          "P" #'go-guru-pointsto   ; What does this point to
          "r" #'go-guru-referrers  ; List references to object
          "e" #'go-guru-whicherrs  ; Which errors
          "w" #'go-guru-what       ; What query
          "c" #'go-guru-callers    ; Show callers of this function
          "C" #'go-guru-callees)   ; Show callees of this function
        (:prefix ("ri" . "imports")
          "a" #'go-import-add
          "r" #'go-remove-unused-imports)
        (:prefix ( "b" . "build")
          :desc "go run ." "r" (λ! (compile "go run ."))
          :desc "go build" "b" (λ! (compile "go build"))
          :desc "go clean" "c" (λ! (compile "go clean")))
        (:prefix ("t" . "test")
          "t" #'+go/test-rerun
          "a" #'+go/test-all
          "s" #'+go/test-single
          "n" #'+go/test-nested)))


(def-package! gorepl-mode
  :commands gorepl-run-load-current-file)


(def-package! company-go
  :when (featurep! :completion company)
  :after go-mode
  :config
  (set-company-backend! 'go-mode 'company-go)
  (setq company-go-show-annotation t))
