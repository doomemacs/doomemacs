;;; lang/lean/config.el -*- lexical-binding: t; -*-

(use-package! nael
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\.lean\'" . nael-mode))
  (add-hook 'nael-mode-hook #'abbrev-mode)
  :config
  (when (featurep! :tools lsp +eglot)
    (add-hook 'nael-mode-hook #'eglot-ensure))

  (sp-with-modes 'nael-mode
    (sp-local-pair "/-" "-/")
    (sp-local-pair "`" "`")
    (sp-local-pair "{" "}")
    (sp-local-pair "«" "»")
    (sp-local-pair "⟨" "⟩")
    (sp-local-pair "⟪" "⟫"))

  (map! :map nael-mode-map
        :localleader
        "a" #'nael-abbrev-help
        "b" #'project-compile
        "e" #'eldoc-doc-buffer
        (:prefix ("s" . "server")
          "r" (cond ((featurep! :tools lsp +eglot) #'eglot-reconnect)
                    ((featurep! :tools lsp) #'lsp-workspace-restart)))))

(use-package! nael-lsp
  :when (and (featurep! :tools lsp) (not (featurep! :tools lsp +eglot)))
  :after nael
  :init
  (add-hook 'nael-mode-hook #'lsp-deferred)
  :config
  (add-hook 'nael-mode-hook #'nael-lsp-configure-when-managed))


;; 
;;; Lean 3

(when (featurep! +v3)
  (after! lean-mode
    (set-lookup-handlers! 'lean-mode
      :definition #'lean-find-definition)
    (sp-with-modes 'lean-mode
      (sp-local-pair "/-" "-/")
      (sp-local-pair "`" "`")
      (sp-local-pair "{" "}")
      (sp-local-pair "«" "»")
      (sp-local-pair "⟨" "⟩")
      (sp-local-pair "⟪" "⟫"))
    (map! :map lean-mode-map
          :localleader
          "g" #'lean-toggle-show-goal
          "n" #'lean-toggle-next-error
          (:prefix ("s" . "server")
            "r" #'lean-server-restart
            "s" #'lean-server-stop
            "v" #'lean-server-switch-version)
          (:prefix ("p" . "leanpkg")
            "t" #'lean-leanpkg-test
            "b" #'lean-leanpkg-build
            "c" #'lean-leanpkg-configure)
          "f" #'lean-fill-placeholder
          "h" #'lean-hole
          "m" #'lean-message-boxes-toggle
          "e" #'lean-execute)))
