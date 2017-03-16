;;; lang/go/config.el

(def-package! go-mode
  :mode "\\.go$"
  :interpreter "go"
  :init
  (add-hook 'go-mode-hook 'flycheck-mode)
  (add-hook! go-mode (add-hook 'before-save-hook 'gofmt-before-save))
  :config
  (set! :build 'go-build 'go-mode '+go/build)
  (set! :company-backend 'go-mode '(company-go company-yasnippet))
  (set! :repl 'go-mode 'gorepl-run)

  (map! :map go-mode-map
        :n "gd" 'godef-jump
        (:localleader
         :n "fh"  'godef-describe
         (:prefix "r"
           :n  "i" 'go-remove-unused-imports
           :nv "f" 'gofmt)
         (:prefix "t"
           :n "r" '+go/test-run-all
           :n "a" '+go/test-run-all
           :n "s" '+go/test-run-package))))


(def-package! company-go :after go-mode)


(def-package! go-eldoc
  :after go-mode
  :init (add-hook 'go-mode-hook 'go-eldoc-setup))


(def-package! gorepl-mode
  :commands (gorepl-run gorepl-run-load-current-file))

