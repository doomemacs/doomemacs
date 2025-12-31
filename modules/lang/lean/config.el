;;; lang/lean/config.el -*- lexical-binding: t; -*-

(use-package! lean-mode  ; lean 3 support
  :when (modulep! +v3)
  :defer t
  :config
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
        "e" #'lean-execute))


(use-package! nael  ; lean 4 support
  :defer t
  :init
  (add-hook 'nael-mode-hook #'abbrev-mode)
  (after! org-src
    (add-to-list 'org-src-lang-modes '("lean" . nael)))
  (after! markdown-mode
    (add-to-list 'markdown-code-lang-modes '("lean" . nael-mode)))
  :config
  (sp-with-modes 'nael-mode
    (sp-local-pair "/-" "-/")
    (sp-local-pair "`" "`")
    (sp-local-pair "{" "}")
    (sp-local-pair "«" "»")
    (sp-local-pair "⟨" "⟩")
    (sp-local-pair "⟪" "⟫"))
  (when (modulep! +lsp)
    (if (modulep! :tools lsp +eglot)
        (setq nael-prepare-lsp nil)
      (setq nael-prepare-eglot nil))
    (add-hook 'nael-mode-local-vars-hook #'lsp! 'append))
  (map! :map nael-mode-map
        :localleader
        "a" #'nael-abbrev-help
        "b" #'project-build      ; REVIEW: redundant with '<leader> p'?
        "e" #'eldoc-doc-buffer)) ; REVIEW: redundant with +lookup/documentation?
