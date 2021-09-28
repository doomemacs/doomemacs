;; -*- no-byte-compile: t; -*-
;;; core/test/test-core.el

(describe "core"
  :var (doom-interactive-p)
  (before-each
    (setq doom-interactive-p nil))

  (describe "initialization"
    (describe "doom-initialize"
      :var (doom-init-p)
      (before-each
        (setq doom-init-p nil))

      (it "initializes once"
        (expect (doom-initialize nil 'noerror))
        (expect (not (doom-initialize nil 'noerror)))
        (expect (not (doom-initialize nil 'noerror)))
        (expect doom-init-p))

      (it "initializes multiple times, if forced"
        (expect (doom-initialize nil 'noerror))
        (expect (not (doom-initialize nil 'noerror)))
        (expect (doom-initialize 'force 'noerror)))

      (describe "package initialization"
        (before-each
          (spy-on 'doom-initialize-packages :and-return-value t))

        (it "initializes packages if core autoload file doesn't exist"
          (let ((doom-autoloads-file "doesnotexist"))
            (expect (doom-initialize nil 'noerror))
          (expect 'doom-initialize-packages :to-have-been-called))

        (it "doesn't initialize packages if core autoload file was loaded"
          (let ((doom-interactive-p t))
            (spy-on 'doom-load-autoloads-file :and-return-value t)
            (doom-initialize nil 'noerror)
            (expect 'doom-load-autoloads-file :to-have-been-called-with doom-package-autoload-file)
            (expect 'doom-initialize-packages :to-have-been-called)))

        (it "initializes packages when forced"
          (doom-initialize 'force 'noerror)
          (expect 'doom-initialize-packages :to-have-been-called)))

      (describe "autoloads files"
        (before-each
          (spy-on 'doom-load-autoloads-file)
          (spy-on 'warn :and-return-value t))

        (it "loads autoloads files"
          (ignore-errors (doom-initialize nil 'noerror))
          (expect 'doom-load-autoloads-file
                  :to-have-been-called-with doom-autoloads-file)
          (expect 'doom-load-autoloads-file
                  :to-have-been-called-with doom-package-autoload-file))

        (it "throws doom-autoload-error when autoload files don't exist"
          (let ((doom-autoloads-file "doesnotexist")
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
    :var (doom-autoloads-file doom-alt-autoload-file result)
    (before-each
      (setq doom-autoloads-file (make-temp-file "doom-autoload" nil ".el"))
      (with-temp-file doom-autoloads-file)
      (byte-compile-file doom-autoloads-file))
    (after-each
      (delete-file doom-autoloads-file)
      (delete-file (byte-compile-dest-file doom-autoloads-file)))

    (it "loads the byte-compiled autoloads file if available"
      (doom-load-autoloads-file doom-autoloads-file)
      (expect (caar load-history) :to-equal-file
              (byte-compile-dest-file doom-autoloads-file))

      (delete-file (byte-compile-dest-file doom-autoloads-file))
      (doom-load-autoloads-file doom-autoloads-file)
      (expect (caar load-history) :to-equal-file doom-autoloads-file))

    (it "returns non-nil if successful"
      (expect (doom-load-autoloads-file doom-autoloads-file)))

    (it "returns nil on failure or error, non-fatally"
      (expect (doom-load-autoloads-file "/does/not/exist") :to-be nil)))

  (describe "doom-load-envvars-file"
    :var (doom-env-file process-environment)
    (before-each
      (setq process-environment nil
            doom-env-file (make-temp-file "doom-env"))
      (with-temp-file doom-env-file
        (insert "A=1\nB=2\nC=3\n")))
    (after-each
      (delete-file doom-env-file))

    (it "throws a file-error if file doesn't exist"
      (expect (doom-load-envvars-file "/tmp/envvardoesnotexist")
              :to-throw 'file-error))

    (it "to fail silently if NOERROR is non-nil"
      (expect (doom-load-envvars-file "/tmp/envvardoesnotexist" 'noerror)
              :not :to-throw))

    (it "returns the new value for `process-environment'"
      (expect (doom-load-envvars-file doom-env-file)
              :to-have-same-items-as '("A" "B" "C")))

    (it "alters environment variables"
      (dolist (key '("A" "B" "C"))
        (expect (getenv key) :not :to-be-truthy))
      (expect (doom-load-envvars-file doom-env-file))
      (expect (getenv "A") :to-equal "1")
      (expect (getenv "B") :to-equal "2")
      (expect (getenv "C") :to-equal "3"))))
