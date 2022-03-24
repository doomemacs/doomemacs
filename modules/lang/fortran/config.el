;;; lang/fortran/config.el -*- lexical-binding: t; -*-

;;
;;; Packages

(use-package! f90
  :defer t
  :config
  ;; --- Compilation --- ;;
  ;; Used by `compile' (SPC c c)
  (setq-hook! 'f90-mode-hook
    compile-command "gfortran "
    compilation-buffer-name-function #'+fortran-compilation-buffer-name-fn)
  (set-popup-rule! "^\\*fortran-compilation" :side 'right :size 0.5 :quit t)

  ;; --- LSP Configuration --- ;;
  (when (featurep! +lsp)
    (setq lsp-clients-fortls-args '("--enable_code_actions" "--hover_signature"))
    (add-hook 'f90-mode-local-vars-hook #'lsp! 'append))

  ;; --- Keybindings --- ;;
  (map! :map f90-mode-map
        :localleader
        (:prefix ("f" . "fpm")
         :desc "fpm build" "b" #'+fortran/fpm-build
         :desc "fpm run"   "r" #'+fortran/fpm-run
         :desc "fpm test"  "t" #'+fortran/fpm-test)
        (:prefix ("g" . "gfortran")
         :desc "compile" "c" #'+fortran/gfortran-compile
         :desc "run"     "r" #'+fortran/gfortran-run)
        :desc "build" "b" #'+fortran/build
        :desc "run"   "r" #'+fortran/run)

  (easy-menu-define f90-menu f90-mode-map "Simpler menu for F90 mode."
    `("F90"
      ["Compile" +fortran/build :active t :help "Compile the Project"]
      ["Run" +fortran/run :active t :help "Run the Executable"]
      ["Test" +fortran/fpm-test :active (+fortran--fpm-toml) :help "Run the Unit Tests"])))

(use-package! fortran
  ;; The `.for' extension is automatically recognized by Emacs and invokes
  ;; `fortran-mode', but not its capital variant `.FOR'. Many old files are
  ;; named the latter way, so we account for that manually here.
  :mode ("\\.FOR$" . fortran-mode)
  :config
  ;; Or else Flycheck will get very mad.
  (setq flycheck-gfortran-language-standard "legacy")

  ;; --- Compilation --- ;;
  ;; Used by `compile' (SPC c c)
  (setq-hook! 'fortran-mode-hook ; TODO These work for f90 but not for fortran.
    compile-command "gfortran -std=legacy "
    compilation-buffer-name-function #'+fortran-compilation-buffer-name-fn)
  (set-popup-rule! "^\\*fortran-compilation" :side 'right :size 0.5 :quit t)

  ;; --- LSP --- ;;
  ;; Strangely, the built-in flycheck support seems to give better hints than the LSP.
  ;; (when (featurep! +lsp)
  ;;   (setq lsp-clients-fortls-args '("--enable_code_actions" "--hover_signature"))
  ;;   (add-hook 'fortran-mode-local-vars-hook #'lsp! 'append)))

  ;; --- Keybindings --- ;;
  (map! :map fortran-mode-map
        :localleader
        (:prefix ("g" . "gfortran")
         :desc "compile" "c" #'+fortran/gfortran-compile
         :desc "run"     "r" #'+fortran/gfortran-run))

  (easy-menu-define fortran-menu fortran-mode-map "Simpler menu for Fortran mode."
    '("Fortran"
      ["Compile" +fortran/gfortran-compile :active t :help "Compile with gfortran"]
      ["Run" +fortran/gfortran-run :active t :help "Run the Executable"])))
