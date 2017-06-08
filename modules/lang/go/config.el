;;; lang/go/config.el -*- lexical-binding: t; -*-

(def-package! go-mode
  :mode "\\.go$"
  :interpreter "go"
  :config
  (setq gofmt-command "goimports")

  (add-hook 'go-mode-hook #'flycheck-mode)

  (if (not (executable-find "goimports"))
      (warn "go-mode: couldn't find goimports; no code formatting/fixed imports on save")
    (add-hook! go-mode (add-hook 'before-save-hook #'gofmt-before-save nil t)))

  (set! :build 'go-build 'go-mode #'+go/build)
  (set! :repl 'go-mode #'gorepl-run)

  (map! :map go-mode-map
        :n "gd" #'go-guru-definition
        :n "gD" #'go-guru-referrers
        (:localleader
          :n "gr"   #'go-play-buffer
          :v "gr"   #'go-play-region
          (:prefix "f"
            :n "i"  #'go-goto-imports
            :n "h"  #'godoc-at-point
            :n "d"  #'go-guru-describe
            :n "v"  #'go-guru-freevars
            :n "I"  #'go-guru-implements
            :n "p"  #'go-guru-peers
            :n "r"  #'go-guru-referrers
            :n "t"  #'go-guru-pointsto
            :n "s"  #'go-guru-callstack
            :n "e"  #'go-guru-whicherrs
            :n "c"  #'go-guru-callers
            :n "C"  #'go-guru-callees)
          (:prefix "r"
            :n  "a" #'go-import-add
            :n  "i" #'go-remove-unused-imports
            :nv "f" #'gofmt)
          (:prefix "t"
            :n "r"  #'+go/test-rerun
            :n "a"  #'+go/test-all
            :n "s"  #'+go/test-single
            :n "n"  #'+go/test-nested))))


(def-package! go-eldoc
  :after go-mode
  :commands go-eldoc-setup
  :config (add-hook 'go-mode-hook #'go-eldoc-setup))


(def-package! go-guru
  :commands (go-guru-describe go-guru-freevars go-guru-implements go-guru-peers
             go-guru-referrers go-guru-definition go-guru-pointsto
             go-guru-callstack go-guru-whicherrs go-guru-callers go-guru-callees
             go-guru-expand-region)
  :config
  (unless (executable-find "guru")
    (warn "go-mode: couldn't find guru, refactoring commands won't work")))


(def-package! gorepl-mode
  :commands (gorepl-run gorepl-run-load-current-file)
  :config
  (unless (executable-find "gore")
    (warn "go-mode: couldn't find gore, REPL support disabled")))


(def-package! company-go
  :init (setq command-go-gocode-command "gocode")
  :when (featurep! :completion company)
  :after go-mode
  :config
  (if (executable-find command-go-gocode-command)
      (set! :company-backend 'go-mode '(company-go))
    (warn "go-mode: couldn't find gocode, code completion won't work")))
