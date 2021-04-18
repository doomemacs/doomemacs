;; -*- no-byte-compile: t; -*-
;;; ../core/test/test-core-ui.el

(describe "core/ui"
  (before-all
    (with-demoted-errors "Import error: %s"
      (require 'core-ui)))

  (describe "doom-protect-fallback-buffer-h"
    :var (kill-buffer-query-functions)
    (before-all
      (setq kill-buffer-query-functions '(doom-protect-fallback-buffer-h)))

    (it "should kill other buffers"
      (expect (kill-buffer (get-buffer-create "a"))))

    (it "shouldn't kill the fallback buffer"
      (expect (not (kill-buffer (doom-fallback-buffer))))))

  (describe "custom hooks"
    (describe "switch hooks"
      :var (before-hook after-hook a b)
      (before-each
        (setq a (switch-to-buffer (get-buffer-create "a"))
              b (get-buffer-create "b"))
        (spy-on 'hook)
        (add-hook 'buffer-list-update-hook #'doom-run-switch-window-hooks-h)
        (add-hook 'focus-in-hook #'doom-run-switch-frame-hooks-h)
        (dolist (fn '(switch-to-buffer display-buffer))
          (advice-add fn :around #'doom-run-switch-buffer-hooks-a)))
      (after-each
        (remove-hook 'buffer-list-update-hook #'doom-run-switch-window-hooks-h)
        (remove-hook 'focus-in-hook #'doom-run-switch-frame-hooks-h)
        (dolist (fn '(switch-to-buffer display-buffer))
          (advice-remove fn #'doom-run-switch-buffer-hooks-a))
        (kill-buffer a)
        (kill-buffer b))

      (describe "switch-buffer"
        :var (doom-switch-buffer-hook)
        (before-each
          (setq doom-switch-buffer-hook '(hook)))
        (after-each
          (setq doom-switch-buffer-hook nil))

        (it "should trigger when switching buffers"
          (switch-to-buffer b)
          (switch-to-buffer a)
          (switch-to-buffer b)
          (expect 'hook :to-have-been-called-times 3))

        (it "should trigger only once on the same buffer"
          (switch-to-buffer b)
          (switch-to-buffer b)
          (switch-to-buffer a)
          (expect 'hook :to-have-been-called-times 2)))


      (describe "switch-window"
        :var (doom-switch-window-hook x y)
        (before-each
          (delete-other-windows)
          (setq x (get-buffer-window a)
                y (save-selected-window (split-window)))
          (with-selected-window y
            (switch-to-buffer b))
          (select-window x)
          (spy-calls-reset 'hook)
          (setq doom-switch-window-hook '(hook)))

        (it "should trigger when switching windows"
          (select-window y)
          (select-window x)
          (select-window y)
          (expect 'hook :to-have-been-called-times 3))

        (it "should trigger only once on the same window"
          (select-window y)
          (select-window y)
          (select-window x)
          (expect 'hook :to-have-been-called-times 2)))


      (xdescribe "switch-frame"
        :var (doom-switch-frame-hook x y)
        (before-each
          (delete-other-windows)
          (setq x (get-buffer-window a)
                y (save-selected-window (split-window)))
          (with-selected-window y
            (switch-to-buffer b))
          (select-window x)
          (spy-calls-reset 'hook)
          (setq doom-switch-window-hook '(hook)))

        (it "should trigger when switching windows"
          (select-window y)
          (select-window x)
          (select-window y)
          (expect 'hook :to-have-been-called-times 3))

        (it "should trigger only once on the same window"
          (select-window y)
          (select-window y)
          (select-window x)
          (expect 'hook :to-have-been-called-times 2))))))
