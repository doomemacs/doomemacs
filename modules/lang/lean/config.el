;;; lang/lean/config.el -*- lexical-binding: t; -*-

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
        "e" #'lean-execute))


(use-package! company-lean
  :when (featurep! :completion company)
  :after lean-mode
  :init
  (advice-add #'company-lean-hook :override #'ignore)
  (set-company-backend! 'lean-mode 'company-lean))
