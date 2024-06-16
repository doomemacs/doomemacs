;; -*- no-byte-compile: t; -*-
;;; ui/popup/test/test-popup.el

(require! :ui popup)

(describe "ui/popup"
  :var (display-buffer-alist
        +popup-default-display-buffer-actions
        +popup--display-buffer-alist
        +popup-defaults
        wconf)

  (before-all
    (delete-other-windows)
    (switch-to-buffer "*scratch*")
    (setq wconf (current-window-configuration))
    (+popup-mode +1))
  (after-all
    (+popup-mode -1))

  (before-each
    (setq display-buffer-alist nil
          +popup--display-buffer-alist nil
          +popup-default-display-buffer-actions '(+popup-display-buffer-stacked-side-window-fn)
          +popup-defaults '(:side bottom :select ignore :ttl nil :slot 1 :vslot 1)))
  (after-each
    (set-window-configuration wconf))

  (describe "set-popup-rule!"
    (it "sets popup rules"
      (set-popup-rule! "does-not-exist" :size 10)
      (let ((rule (cdr (assoc "does-not-exist" display-buffer-alist))))
        (expect rule :to-contain '(+popup-buffer))
        (expect rule :to-contain '(size . 10))))
    (it "shadows old rules"
      (set-popup-rule! "a" :size 10)
      (set-popup-rule! "a" :size 20)
      (expect (cdr (assoc "a" display-buffer-alist))
              :to-contain '(size . 20)))
    (it "resolves to defaults"
      (let ((+popup-defaults '(:size 5)))
        (set-popup-rule! "a")
        (expect (cdr (assoc "a" display-buffer-alist))
                :to-contain '(size . 5)))))

  (describe "popup rules"
    :var (origin a b c d e f g h i)
    (before-all (setq origin (current-buffer)))
    (before-each
      (dolist (name '(a b c d e f g h i))
        (set name (get-buffer-create (symbol-name name)))))
    (after-each
      (let (kill-buffer-query-functions kill-buffer-hook)
        (dolist (x (list a b c d e f g h i))
          (ignore-errors (delete-window (get-buffer-window x)))
          (kill-buffer x))))

    (describe "slot positioning"
      (before-each
        (set-popup-rules!
          '(("a" :slot 1 :vslot 1)
            ("b" :slot 2 :vslot 1)
            ("c" :slot 1 :vslot 2)
            ("d" :slot 2 :vslot 2)
            ("e" :slot 1 :vslot 3)
            ("f" :slot 1 :vslot 3)
            ("g" :slot 2 :vslot 3)
            ("h" :slot 2 :vslot 3)
            ("i"))))

      (it "replaces popups with the same slots"
        (mapc #'display-buffer (list e f g h))
        (expect (length (+popup-windows)) :to-be 2))

      (it "replaces popups among multiple that have the same slots"
        (let ((first  (display-buffer a))
              (second (display-buffer b))
              (third  (display-buffer e))
              (fourth (display-buffer f)))
          (expect (+popup-windows) :to-have-same-items-as
                  (list first second fourth))))

      (describe ":slot"
        (it "opens left of others if lower"
          (let ((first  (display-buffer b))
                (second (display-buffer a)))
            (expect (length (+popup-windows)) :to-be 2)
            (expect (window-in-direction 'left first t)
                    :to-equal second)))
        (it "opens right of others if higher"
          (let ((first  (display-buffer a))
                (second (display-buffer b)))
            (expect (length (+popup-windows)) :to-be 2)
            (expect (window-in-direction 'right first t)
                    :to-equal second)))
        (it "obeys default :slot"
          (let ((window (display-buffer i)))
            (expect (window-parameter window 'window-slot)  :to-be 1)
            (expect (window-parameter window 'window-vslot) :to-be 1))))

      (describe ":vslot"
        ;; TODO Implement this, somehow
        (xit "opens lower :vslot popups above others"
          (let ((first  (display-buffer c))
                (second (display-buffer a)))
            (expect (length (+popup-windows)) :to-be 2)
            (expect (window-in-direction 'above first t)
                    :to-equal second)))
        (it "opens higher :vslot popups below others"
          (let ((first  (display-buffer c))
                (second (display-buffer e)))
            (expect (length (+popup-windows)) :to-be 2)
            (expect (window-in-direction 'below first t)
                    :to-equal second)))))

    (describe ":select"
      (it "selects the popup if non-nil"
        (set-popup-rule! "^a$" :select t)
        (display-buffer a)
        (expect (current-buffer) :to-equal a))
      (it "selects the originating window if nil"
        (set-popup-rule! "^a$" :select nil)
        (display-buffer a)
        (expect (current-buffer) :to-equal origin))
      (it "fall back to base selection if passed #'ignore"
        (spy-on 'ignore)
        (set-popup-rule! "^a$" :select #'ignore)
        (save-window-excursion
          (display-buffer a)
          (expect (current-buffer) :to-equal origin))
        (save-window-excursion
          (pop-to-buffer a)
          (expect (current-buffer) :to-equal a))
        (expect 'ignore :to-have-been-called-times 2)))

    (describe ":modeline"
      (it "disables the mode-line if nil"
        (set-popup-rule! "a" :modeline nil :select t)
        (display-buffer a)
        (expect mode-line-format :to-be nil))
      (it "uses the default mode-line if t"
        (set-popup-rule! "a" :modeline t :select t)
        (display-buffer a)
        (expect mode-line-format :to-equal (default-value 'mode-line-format)))
      (it "uses a predefined mode-line if passed a symbol"
        (set-popup-rule! "a" :modeline '("x") :select t)
        (display-buffer a)
        (expect mode-line-format :to-equal '("x")))
      (it "runs the handler if passed a function"
        (set-popup-rule! "a" :modeline (lambda () (setq mode-line-format '("x"))) :select t)
        (display-buffer a)
        (expect mode-line-format :to-equal '("x"))))

    ;; TODO
    (xdescribe ":autosave")

    (describe ":quit"
      (it "will close from anywhere if :quit = t"
        (set-popup-rule! "a" :quit t)
        (save-window-excursion
          (display-buffer a)
          (call-interactively #'+popup/close-all)
          (expect (get-buffer-window a) :to-be nil))
        (save-window-excursion
          (pop-to-buffer a)
          (call-interactively #'+popup/close)
          (expect (get-buffer-window a) :to-be nil)))
      (it "will only close from outside if :quit = 'other"
        (set-popup-rule! "a" :quit 'other)
        (save-window-excursion
          (display-buffer a)
          (call-interactively #'+popup/close-all)
          (expect (get-buffer-window a) :to-be nil))
        (save-window-excursion
          (pop-to-buffer a)
          (call-interactively #'+popup/close)
          (expect (get-buffer-window a))))
      (it "will only close from inside if :quit = 'current"
        (set-popup-rule! "a" :quit 'current)
        (save-window-excursion
          (display-buffer a)
          (call-interactively #'+popup/close-all)
          (expect (get-buffer-window a)))
        (save-window-excursion
          (pop-to-buffer a)
          (call-interactively #'+popup/close)
          (expect (get-buffer-window a) :to-be nil)))
      (it "never close a if :quit = nil"
        (set-popup-rule! "a" :quit nil)
        (save-window-excursion
          (display-buffer a)
          (call-interactively #'+popup/close-all)
          (expect (get-buffer-window a)))
        (save-window-excursion
          (pop-to-buffer a)
          (call-interactively #'+popup/close)
          (expect (get-buffer-window a)))))

    ;; TODO
    (xdescribe ":ttl")
    (xdescribe ":size")
    (xdescribe ":width")
    (xdescribe ":height")
    (xdescribe ":side")
    (xdescribe ":actions"))

  ;; TODO
  (xdescribe "predicate functions"
    (describe "buffer-p")
    (describe "window-p"))

  ;; TODO
  (xdescribe "save-popups!")
  (xdescribe "with-popup-rules!"))
