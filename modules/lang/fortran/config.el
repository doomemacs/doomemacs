;;; lang/fortran/config.el -*- lexical-binding: t; -*-

;;
;;; Packages

(use-package! f90
  :config
  ;; Used by `compile' (SPC c c)
  (setq compile-command "gfortran ")

  ;; --- LSP Configuration --- ;;
  (when (featurep! +lsp)
    (setq lsp-clients-fortls-args '("--enable_code_actions" "--hover_signature"))
    (add-hook 'f90-mode-local-vars-hook #'lsp!))

  ;; --- Keybindings --- ;;
  (map! :map f90-mode-map
        :localleader
        (:prefix ("f" . "fpm")
         :desc "fpm build" "b" #'+fortran/fpm-build
         :desc "fpm run"   "r" #'+fortran/fpm-run
         :desc "fpm test"  "t" #'+fortran/fpm-test)
        :desc "compile (gfortran)" "c" #'+fortran/gfortran-compile
        :desc "run (gfortran)"     "r" #'+fortran/gfortran-run))

(use-package! fortran
  ;; The `.for' extension is automatically recognized by Emacs and invokes
  ;; `fortran-mode', but not its capital variant `.FOR'. Many old files are
  ;; named the latter way, so we account for that manually here.
  :mode ("\\.FOR$" . fortran-mode)
  :config
  ;; Or else Flycheck will get very mad.
  (setq flycheck-gfortran-language-standard "legacy")

  ;; Used by `compile' (SPC c c)
  (setq compile-command "gfortran -std=legacy ")

  ;; Strangely, the built-in flycheck support seems to give better hints than the LSP.
  ;; (when (featurep! +lsp)
  ;;   (setq lsp-clients-fortls-args '("--enable_code_actions" "--hover_signature"))
  ;;   (add-hook 'fortran-mode-local-vars-hook #'lsp!)))

  ;; --- Keybindings --- ;;
  (map! :map fortran-mode-map
        :localleader
        :desc "compile (gfortran)" "c" #'+fortran/gfortran-compile
        :desc "run (gfortran)"     "r" #'+fortran/gfortran-run))
