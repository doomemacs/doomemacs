;; -*- no-byte-compile: t; -*-
;;; ../core/test/test-core-ui.el

(describe "core/ui"
  (describe "doom|set-mode-name"
    :var (doom-major-mode-names after-change-major-mode-hook)
    (before-all
      (setq doom-major-mode-names
            '((text-mode . "abc")
              (lisp-mode . (lambda () "xyz"))
              (js-mode . t))
            after-change-major-mode-hook '(doom|set-mode-name)))

    (it "changes `mode-name' to match a given string"
      (text-mode)
      (expect mode-name :to-equal "abc"))

    (it "changes `mode-name' to the result of a function"
      (lisp-mode)
      (expect mode-name :to-equal "xyz"))

    (it "should fail if given an invalid name"
      (expect (js-mode) :to-throw 'error)))


  (describe "doom|protect-visible-buffers"
    :var (kill-buffer-query-functions wconf a b)
    (before-each
      (setq a (switch-to-buffer (get-buffer-create "a"))
            b (get-buffer-create "b")
            kill-buffer-query-functions '(doom|protect-visible-buffers)
            wconf (current-window-configuration))
      (delete-other-windows))

    (after-each
      (let (kill-buffer-query-functions kill-buffer-hook)
        (kill-buffer a)
        (kill-buffer b))
      (set-window-configuration wconf))

    (it "shouldn't kill buffers that are visible in more than one window"
      (with-temp-buffer-window
       (switch-to-buffer a) (split-window)
       (switch-to-buffer b) (split-window)
       (switch-to-buffer a)
       (expect (kill-buffer) :to-be nil)

       (select-window (get-buffer-window b))
       (expect (kill-buffer)))))


  (describe "doom|protect-fallback-buffer"
    :var (kill-buffer-query-functions a b)
    (before-all
      (setq kill-buffer-query-functions '(doom|protect-fallback-buffer)))

    (it "should kill other buffers"
      (expect (kill-buffer (get-buffer-create "a"))))

    (it "shouldn't kill the fallback buffer"
      (expect (not (kill-buffer (doom-fallback-buffer))))))


  (describe "switch hooks"
    :var (before-hook after-hook a b)
    (before-each
      (setq a (switch-to-buffer (get-buffer-create "a"))
            b (get-buffer-create "b"))
      (spy-on 'before-hook)
      (spy-on 'after-hook)
      (doom|init-custom-hooks))
    (after-each
      (doom|init-custom-hooks 'disable)
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
        (expect 'after-hook :to-have-been-called-times 2)))))

