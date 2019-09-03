;; -*- no-byte-compile: t; -*-
;;; core/test/test-core.el

(describe "core"
  :var (doom-interactive-mode)
  (before-each
    (setq doom-interactive-mode nil))

  (describe "initialization"
    (describe "doom-initialize"
      :var (doom-init-p)
      (before-each
        (setq doom-init-p nil))

      (it "initializes once"
        (expect (doom-initialize))
        (expect (not (doom-initialize)))
        (expect (not (doom-initialize)))
        (expect doom-init-p))

      (it "initializes multiple times, if forced"
        (expect (doom-initialize))
        (expect (not (doom-initialize)))
        (expect (doom-initialize 'force)))

      (describe "package initialization"
        (before-each
          (spy-on 'doom-initialize-packages :and-return-value t))

        (it "initializes packages if core autoload file doesn't exist"
          (let ((doom-autoload-file "doesnotexist"))
            (doom-initialize))
          (expect 'doom-initialize-packages :to-have-been-called))

        (it "doesn't initialize packages if core autoload file was loaded"
          (let ((doom-interactive-mode t))
            (spy-on 'doom-load-autoloads-file :and-return-value t)
            (doom-initialize)
            (expect 'doom-load-autoloads-file :to-have-been-called-with doom-package-autoload-file)
            (expect 'doom-initialize-packages :to-have-been-called)))

        (it "initializes packages when forced"
          (doom-initialize 'force)
          (expect 'doom-initialize-packages :to-have-been-called)))

      (describe "autoloads files"
        (before-each
          (spy-on 'doom-load-autoloads-file)
          (spy-on 'warn :and-return-value t))

        (it "loads autoloads file"
          (let ((doom-interactive-mode t))
            (ignore-errors (doom-initialize)))
          (expect 'doom-load-autoloads-file
                  :to-have-been-called-with doom-autoload-file)
          (expect 'doom-load-autoloads-file
                  :to-have-been-called-with doom-package-autoload-file))

        (it "does not load package autoloads file if noninteractive"
          (doom-initialize)
          (expect 'doom-load-autoloads-file
                  :to-have-been-called-with doom-autoload-file)
          (expect 'doom-load-autoloads-file
                  :not :to-have-been-called-with doom-package-autoload-file))

        (it "throws doom-autoload-error in interactive session where autoload files don't exist"
          (let ((doom-interactive-mode t)
                (doom-autoload-file "doesnotexist")
                (doom-package-autoload-file "doesnotexist"))
            (expect (doom-initialize) :to-throw 'doom-autoload-error)))))

    (describe "doom-initialize-core"
      (before-each
        (spy-on 'require))

      (it "loads all doom core libraries"
        (doom-initialize-core)
        (expect 'require :to-have-been-called-with 'core-keybinds)
        (expect 'require :to-have-been-called-with 'core-ui)
        (expect 'require :to-have-been-called-with 'core-projects)
        (expect 'require :to-have-been-called-with 'core-editor))))

  (describe "doom-load-autoloads-file"
    (before-each
      (spy-on 'load :and-return-value t))

    (it "loads the autoloads file"
      (doom-load-autoloads-file doom-autoload-file)
      (expect 'load :to-have-been-called-with (file-name-sans-extension doom-autoload-file)
              'noerror 'nomessage)))

  (describe "doom-load-envvars-file"
    :var (envvarfile process-environment)
    (before-each
      (setq process-environment (copy-sequence process-environment))
      (with-temp-file doom-env-file
        (insert "\n\n\nA=1\nB=2\nC=3\n")))
    (after-each
      (delete-file doom-env-file))

    (it "throws a file-error if file doesn't exist"
      (expect (doom-load-envvars-file "/tmp/envvardoesnotexist")
              :to-throw 'file-error))

    (it "to fail silently if NOERROR is non-nil"
      (expect (doom-load-envvars-file "/tmp/envvardoesnotexist" 'noerror)
              :not :to-throw))

    (it "loads a well-formed envvar file"
      (expect (getenv "A") :not :to-be-truthy)
      (expect (doom-load-envvars-file doom-env-file)
              :to-equal '(("A" . "1") ("B" . "2") ("C" . "3")))
      (expect (getenv "A") :to-equal "1"))

    (it "fails on an invalid envvar file"
      (with-temp-file doom-env-file (insert "A=1\nB=2\nC=3\n"))
      (expect (doom-load-envvars-file doom-env-file) :to-throw))))
