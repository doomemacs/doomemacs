;; -*- no-byte-compile: t; -*-
;;; core/test/test-core.el

(describe "core"
  (xdescribe "initialize"
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

  (xdescribe "initialize-packages"
    (before-each (spy-on 'quelpa-setup-p))

    (it "initializes package.el once, unless forced" )
    (it "initializes quelpa once, unless forced" )
    (it "initializes doom-packages once, unless forced" ))

  (xdescribe "initialize-modules"
    (it "loads private init.el once, unless forced" ))

  (xdescribe "initialize-autoloads"
    (it "loads autoloads file" )
    (it "ignores autoloads file if cleared" ))

  (describe "custom hooks"
    (describe "switch hooks"
      :var (before-hook after-hook a b)
      (before-each
        (setq a (switch-to-buffer (get-buffer-create "a"))
              b (get-buffer-create "b"))
        (spy-on 'before-hook)
        (spy-on 'after-hook)
        (doom|init-switch-hooks))
      (after-each
        (doom|init-switch-hooks 'disable)
        (kill-buffer a)
        (kill-buffer b))

      (describe "switch-buffer"
        :var (doom-exit-buffer-hook
              doom-enter-buffer-hook)
        (before-each
          (setq doom-exit-buffer-hook '(before-hook)
                doom-enter-buffer-hook '(after-hook)))
        (after-each
          (setq doom-exit-buffer-hook nil
                doom-enter-buffer-hook nil))

        (it "should trigger when switching buffers"
          (switch-to-buffer b)
          (switch-to-buffer a)
          (switch-to-buffer b)
          (expect 'before-hook :to-have-been-called-times 3)
          (expect 'after-hook :to-have-been-called-times 3))

        (it "should trigger only once on the same buffer"
          (switch-to-buffer b)
          (switch-to-buffer b)
          (switch-to-buffer a)
          (expect 'before-hook :to-have-been-called-times 2)
          (expect 'after-hook :to-have-been-called-times 2)))


      (describe "switch-window"
        :var (doom-exit-window-hook
              doom-enter-window-hook
              x y)
        (before-each
          (delete-other-windows)
          (setq x (get-buffer-window a)
                y (save-selected-window (split-window)))
          (with-selected-window y
            (switch-to-buffer b))
          (select-window x)
          (spy-calls-reset 'before-hook)
          (spy-calls-reset 'after-hook)
          (setq doom-exit-window-hook '(before-hook)
                doom-enter-window-hook '(after-hook)))

        (it "should trigger when switching windows"
          (select-window y)
          (select-window x)
          (select-window y)
          (expect 'before-hook :to-have-been-called-times 3)
          (expect 'after-hook :to-have-been-called-times 3))

        (it "should trigger only once on the same window"
          (select-window y)
          (select-window y)
          (select-window x)
          (expect 'before-hook :to-have-been-called-times 2)
          (expect 'after-hook :to-have-been-called-times 2))))))
