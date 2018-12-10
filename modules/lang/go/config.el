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
        :n "e" #'go-play-buffer
        :v "e" #'go-play-region
        :n "i" #'go-goto-imports      ; Go to imports
        (:desc "guru" :prefix "h"
          :n "." #'godoc-at-point     ; Lookup in godoc
          :n "d" #'go-guru-describe   ; Describe this
          :n "v" #'go-guru-freevars   ; List free variables
          :n "i" #'go-guru-implements ; Implements relations for package types
          :n "p" #'go-guru-peers      ; List peers for channel
          :n "P" #'go-guru-pointsto   ; What does this point to
          :n "r" #'go-guru-referrers  ; List references to object
          :n "e" #'go-guru-whicherrs  ; Which errors
          :n "w" #'go-guru-what       ; What query
          :n "c" #'go-guru-callers    ; Show callers of this function
          :n "C" #'go-guru-callees)   ; Show callees of this function
        (:desc "imports" :prefix "r"
          :n "ia" #'go-import-add
          :n "ir" #'go-remove-unused-imports)
        (:desc "build" :prefix "b"
          :desc "go run"     :n "r" (λ! (compile "go run ."))
          :desc "go build"   :n "b" (λ! (compile "go build"))
          :desc "go compile" :n "c" (λ! (compile "go clean")))
        (:desc "test" :prefix "t"
          :n "t" #'+go/test-rerun
          :n "a" #'+go/test-all
          :n "s" #'+go/test-single
          :n "n" #'+go/test-nested)))


(def-package! gorepl-mode
  :commands gorepl-run-load-current-file)


(def-package! company-go
  :when (featurep! :completion company)
  :after go-mode
  :config
  (set-company-backend! 'go-mode 'company-go)
  (setq company-go-show-annotation t))
