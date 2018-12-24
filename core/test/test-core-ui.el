;; -*- no-byte-compile: t; -*-
;;; ../core/test/test-core-ui.el

(describe "core/ui"
  (describe "doom|protect-visible-buffer"
    :var (kill-buffer-query-functions wconf a b)
    (before-each
      (setq a (switch-to-buffer (get-buffer-create "a"))
            b (get-buffer-create "b")
            kill-buffer-query-functions '(doom|protect-visible-buffer)
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

