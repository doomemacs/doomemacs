;;; lang/go/config.el -*- lexical-binding: t; -*-

;;
;;; Packages

(defun +go-common-config (mode)
  (set-docsets! mode "Go")
  (set-repl-handler! mode #'gorepl-run)
  (set-lookup-handlers! mode
    :documentation #'godoc-at-point)

  (when (modulep! +lsp)
    (add-hook (intern (format "%s-local-vars-hook" mode)) #'lsp! 'append))

  (map! :map ,(intern (format "%s-map" mode))
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


(after! go-mode
  (+go-common-config 'go-mode))


(use-package! go-ts-mode  ; 29.1+ only
  :when (modulep! +tree-sitter)
  :mode ("/go\\.mod\\'" . go-mod-ts-mode-maybe)
  :init
  (set-tree-sitter! 'go-mode 'go-ts-mode
    `((go :url "https://github.com/tree-sitter/tree-sitter-go"
          :rev ,(if (< (treesit-library-abi-version) 15) "v0.20.0" "v0.25.0"))))
  (set-tree-sitter! nil 'go-mod-ts-mode
    '((gomod :url "https://github.com/camdencheek/tree-sitter-go-mod"
             :commit "3b01edce2b9ea6766ca19328d1850e456fde3103")))
  (when (fboundp 'go-work-ts-mode)  ; 30.1+ only
    (add-to-list 'auto-mode-alist '("/go\\.work\\'" . go-work-ts-mode-maybe))
    (set-tree-sitter! nil 'go-work-ts-mode
      '((gowork :url "https://github.com/omertuc/tree-sitter-go-work"
                :commit "949a8a470559543857a62102c84700d291fc984c"))))
  :config
  (+go-common-config 'go-ts-mode))


(use-package! gorepl-mode
  :commands gorepl-run-load-current-file)


(use-package! flycheck-golangci-lint
  :when (modulep! :checkers syntax -flymake)
  :hook (go-mode . flycheck-golangci-lint-setup)
  :hook (go-ts-mode . flycheck-golangci-lint-setup))
