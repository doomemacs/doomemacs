;;; lang/lean/config.el -*- lexical-binding: t; -*-

(use-package! company-lean
  :when (featurep :completion 'company)
  :after lean-mode
  :config (set-company-backend! 'lean-mode 'company-lean))

(use-package! lean-mode
  :config
  (set-lookup-handlers! 'lean-mode
    :definition #'lean-find-definition)
  (sp-with-modes 'lean-mode
    (sp-local-pair "/-" "-/")
    (sp-local-pair "`'" nil :actions :rem)
    (sp-local-pair "{" "}")
    (sp-local-pair "«" "»"))
  (map!
   :map lean-mode-map
   :localleader
   "g" #'lean-toggle-show-goal
   "n" #'lean-toggle-next-error
   (:prefix "s"
     "r" #'lean-server-restart
     "s" #'lean-server-stop
     "v" #'lean-server-switch-version)
   (:prefix "p"
     "t" #'lean-leanpkg-test
     "b" #'lean-leanpkg-build
     "c" #'lean-leanpkg-configure)
   "f" #'lean-fill-placeholder
   "h" #'lean-hole
   "m" #'lean-message-boxes-toggle
   "e" #'lean-execute))
