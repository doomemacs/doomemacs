;; -*- no-byte-compile: t; -*-
;;; core/test/test-core.el

(xdescribe "core"
  (describe "initialize"
    :var (doom-init-p doom-init-modules-p doom-private-dir)
    (before-each
      (setq doom-init-p nil
            doom-init-modules-p nil
            doom-private-dir doom-emacs-dir)

      (spy-on 'require)
      (spy-on 'load)
      (spy-on 'doom-reload-doom-autoloads)
      (spy-on 'doom-reload-package-autoloads)
      (spy-on 'doom-initialize-autoloads)
      (spy-on 'doom-ensure-core-directories)
      (spy-on 'doom-ensure-core-packages)
      (spy-on 'doom-ensure-packages-initialized)
      (spy-on 'doom-ensure-same-emacs-version-p))

    (describe "in interactive session"
      :var (noninteractive)
      (before-each (setq noninteractive t))

      (it "initializes once, unless forced")
      (it "does not initialize on consecutive invokations")
      (it "loads all core libraries" )
      (it "loads autoloads file" )
      (it "does not load autoloads file if forced" )
      (it "regenerates missing autoloads" ))

    (describe "in non-interactive session"
      :var (noninteractive)
      (before-each (setq noninteractive nil))

      (it "initializes once, unless forced")
      (it "does not initialize on consecutive invokations")
      (it "does not load all core libraries" )
      (it "loads autoloads file" )
      (it "does not load autoloads file if forced" )
      (it "does not regenerate missing autoloads" )))

  (describe "initialize-packages"
    (before-each (spy-on 'quelpa-setup-p))

    (it "initializes package.el once, unless forced" )
    (it "initializes quelpa once, unless forced" )
    (it "initializes doom-packages once, unless forced" ))

  (describe "initialize-modules"
    (it "loads private init.el once, unless forced" ))

  (describe "initialize-autoloads"
    (it "loads autoloads file" )
    (it "ignores autoloads file if cleared" )))
