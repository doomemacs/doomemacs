;;; lang/lean4/config.el -*- lexical-binding: t; -*-

(after! lean4-mode
  (sp-with-modes 'lean-mode
    (sp-local-pair "/-" "-/")
    (sp-local-pair "`" "`")
    (sp-local-pair "{" "}")
    (sp-local-pair "«" "»")
    (sp-local-pair "⟨" "⟩")
    (sp-local-pair "⟪" "⟫"))
	(map! :map lean4-mode-map
		:localleader
		:desc "Execute"               "R" #'lean4-execute
		:desc "Execute in standalone" "r" #'lean4-std-exe
		:desc "Toggle info buffer"    "t" #'lean4-toggle-info
		(:prefix ("e" . "Error")
		 :desc "Previous error"       "p" #'flycheck-previous-error
		 :desc "Next error"           "n" #'flycheck-next-error
		 :desc "List error"           "l" #'flycheck-list-errors
		 )
		:desc "Lake build"            "b" #'lean4-lake-build
		(:prefix ("p" . "leanpkg")
		 :desc "Test"                 "t" #'lean4-leanpkg-test
		 :desc "Build"                "b" #'lean4-leanpkg-build
		 :desc "Configure"            "c" #'lean4-leanpkg-configure
		 )
		))
