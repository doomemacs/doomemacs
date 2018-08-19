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
      (expect (not (kill-buffer (doom-fallback-buffer)))))))

