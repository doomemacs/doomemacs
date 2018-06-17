;; -*- no-byte-compile: t; -*-
;;; core/test/test-core-lib.el

(describe "core/lib"
  ;; --- Helpers ----------------------------
  (describe "doom-unquote"
    (it "unquotes a quoted form"
      (expect (doom-unquote '(quote hello)) :to-be 'hello))
    (it "unquotes nested-quoted forms"
      (expect (doom-unquote '(quote (quote (a b c)))) :to-equal '(a b c)))
    (it "unquotes function-quoted forms"
      (expect (doom-unquote '(function a)) :to-be 'a))
    (it "does nothing to unquoted forms"
      (expect (doom-unquote 'hello) :to-be 'hello)))

  (describe "doom-enlist"
    (it "creates a list out of non-lists"
      (expect (doom-enlist 'a) :to-equal '(a)))
    (it "does nothing to lists"
      (expect (doom-enlist '(a)) :to-equal '(a))))


  ;; --- Macros -----------------------------
  (describe "hooks"
    (describe "add-hook!"
      :var (fake-mode-hook other-mode-hook some-mode-hook)
      (before-each
        (setq fake-mode-hook '(first-hook)
              other-mode-hook nil
              some-mode-hook '(first-hook second-hook)))

      (it "adds one-to-one hook"
        (add-hook! fake-mode #'hook-2)
        (add-hook! 'fake-mode-hook #'hook-1)
        (expect fake-mode-hook :to-equal '(hook-1 hook-2 first-hook)))

      (it "adds many-to-one hook"
        (add-hook! (fake-mode other-mode some-mode) #'hook-2)
        (add-hook! '(fake-mode-hook other-mode-hook some-mode-hook) #'hook-1)
        (add-hook! :append (fake-mode other-mode some-mode) #'last-hook)
        (expect fake-mode-hook  :to-equal '(hook-1 hook-2 first-hook last-hook))
        (expect other-mode-hook :to-equal '(hook-1 hook-2 last-hook))
        (expect some-mode-hook  :to-equal '(hook-1 hook-2 first-hook second-hook last-hook)))

      (it "adds many-to-many hooks and preserve provided order"
        (add-hook! (fake-mode other-mode some-mode) #'(hook-3 hook-4))
        (add-hook! '(fake-mode-hook other-mode-hook some-mode-hook) #'(hook-1 hook-2))
        (add-hook! :append '(fake-mode-hook other-mode-hook some-mode-hook) #'(last-hook-1 last-hook-2))
        (expect fake-mode-hook  :to-equal '(hook-1 hook-2 hook-3 hook-4 first-hook last-hook-1 last-hook-2))
        (expect other-mode-hook :to-equal '(hook-1 hook-2 hook-3 hook-4 last-hook-1 last-hook-2))
        (expect some-mode-hook  :to-equal '(hook-1 hook-2 hook-3 hook-4 first-hook second-hook last-hook-1 last-hook-2)))

      (it "adds implicit lambda to one hook"
        (add-hook! fake-mode (progn))
        (add-hook! 'other-mode-hook (ignore))
        (add-hook! :append 'some-mode-hook (ignore))
        (expect (caar fake-mode-hook) :to-be 'lambda)
        (expect (caar other-mode-hook) :to-be 'lambda)
        (expect (caar (last other-mode-hook)) :to-be 'lambda)))

    (describe "remove-hook!"
      :var (fake-mode-hook)
      (before-each
        (setq fake-mode-hook '(first-hook second-hook third-hook fourth-hook)))
      (it "removes one hook"
        (remove-hook! fake-mode #'third-hook)
        (remove-hook! 'fake-mode-hook #'second-hook)
        (expect fake-mode-hook :to-equal '(first-hook fourth-hook)))
      (it "removes multiple hooks"
        (remove-hook! fake-mode #'(first-hook third-hook))
        (remove-hook! 'fake-mode-hook #'(second-hook fourth-hook))
        (expect fake-mode-hook :to-be nil))))

  (describe "add-transient-hook!"
    (it "adds a transient function to hooks"
      (let (hooks value)
        (add-transient-hook! 'hooks (setq value t))
        (run-hooks 'hooks)
        (expect value)
        (expect hooks :to-be nil)))
    (it "advises a function with a transient advisor"
      (let (value)
        (add-transient-hook! #'ignore (setq value (not value)))
        (ignore t)
        (expect value)
        ;; repeat to ensure it was only run once
        (ignore t)
        (expect value))))

  (xdescribe "associate!"))  ; TODO
