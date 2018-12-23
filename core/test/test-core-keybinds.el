;; -*- no-byte-compile: t; -*-
;;; core/test/test-core-keybinds.el

(buttercup-define-matcher :to-bind (src result)
  ;; TODO Add expect messages
  (equal (caddr (macroexpand-1 (funcall src)))
         (funcall result)))

(describe "core/keybinds"
  (describe "map!"
    :var (doom--map-evil-p doom-map-states)
    (before-each
      (setq doom--map-evil-p t
            doom-map-states '((:n . normal)
                              (:v . visual)
                              (:i . insert)
                              (:e . emacs)
                              (:o . operator)
                              (:m . motion)
                              (:r . replace))))

    (describe "Single keybinds"
      (it "binds a global key"
        (expect '(map! "C-." #'a) :to-bind '(general-define-key "C-." #'a)))

      (it "binds a key in one evil state"
        (dolist (state doom-map-states)
          (expect `(map! ,(car state) "C-." #'a)
                  :to-bind `(general-define-key :states ',(cdr state) "C-." #'a))))

      (it "binds a key in multiple evil states"
        (expect `(map! :nvi "C-." #'a)
                :to-bind
                '(progn (general-define-key :states 'insert "C-." #'a)
                        (general-define-key :states 'visual "C-." #'a)
                        (general-define-key :states 'normal "C-." #'a))))

      (it "binds evil keybinds together with global keybinds"
        (expect '(map! :ng "C-." #'a)
                :to-bind
                '(progn
                   (general-define-key :states 'normal "C-." #'a)
                   (general-define-key "C-." #'a)))))

    (describe "Multiple keybinds"
      (it "binds global keys and preserves order"
        (expect '(map! "C-." #'a "C-," #'b "C-/" #'c)
                :to-bind
                '(general-define-key "C-." #'a "C-," #'b "C-/" #'c)))

      (it "binds multiple keybinds in an evil state and preserve order"
        (dolist (state doom-map-states)
          (expect `(map! ,(car state) "a" #'a
                         ,(car state) "b" #'b
                         ,(car state) "c" #'c)
                  :to-bind
                  `(general-define-key :states ',(cdr state)
                                       "a" #'a
                                       "b" #'b
                                       "c" #'c))))

      (it "binds multiple keybinds in different evil states"
        (expect `(map! :n "a" #'a
                       :n "b" #'b
                       :n "e" #'e
                       :v "c" #'c
                       :i "d" #'d)
                :to-bind
                `(progn (general-define-key :states 'insert "d" #'d)
                        (general-define-key :states 'visual "c" #'c)
                        (general-define-key :states 'normal "a" #'a "b" #'b "e" #'e))))

      (it "groups multi-state keybinds while preserving same-group key order"
        (expect `(map! :n "a" #'a
                       :v "c" #'c
                       :n "b" #'b
                       :i "d" #'d
                       :n "e" #'e)
                :to-bind
                `(progn (general-define-key :states 'insert "d" #'d)
                        (general-define-key :states 'visual "c" #'c)
                        (general-define-key :states 'normal "a" #'a "b" #'b "e" #'e))))

      (it "binds multiple keybinds in multiple evil states"
        (expect `(map! :nvi "a" #'a
                       :nvi "b" #'b
                       :nvi "c" #'c)
                :to-bind
                '(progn (general-define-key :states 'insert "a" #'a "b" #'b "c" #'c)
                        (general-define-key :states 'visual "a" #'a "b" #'b "c" #'c)
                        (general-define-key :states 'normal "a" #'a "b" #'b "c" #'c)))))

    (describe "Nested keybinds"
      (it "binds global keys"
        (expect '(map! "C-." #'a
                       ("C-a" #'b)
                       ("C-x" #'c))
                :to-bind
                '(progn
                   (general-define-key "C-." #'a)
                   (general-define-key "C-a" #'b)
                   (general-define-key "C-x" #'c))))

      (it "binds nested evil keybinds"
        (expect '(map! :n "C-." #'a
                       (:n "C-a" #'b)
                       (:n "C-x" #'c))
                :to-bind
                '(progn
                   (general-define-key :states 'normal "C-." #'a)
                   (general-define-key :states 'normal "C-a" #'b)
                   (general-define-key :states 'normal "C-x" #'c))))

      (it "binds global keybinds in between evil keybinds"
        (expect '(map! :n "a" #'a
                          "b" #'b
                       :n "c" #'c)
                :to-bind
                '(progn (general-define-key "b" #'b)
                        (general-define-key :states 'normal "a" #'a "c" #'c)))))

    ;;
    (describe "Properties"
      (describe ":after"
        (it "wraps `general-define-key' in a `after!' block"
          (dolist (form '((map!  :after helm "a" #'a "b" #'b)
                          (map! (:after helm "a" #'a "b" #'b))))
            (expect form :to-bind '(after! helm (general-define-key "a" #'a "b" #'b))))
          (expect '(map! "a" #'a (:after helm "b" #'b "c" #'c))
                  :to-bind
                  '(progn
                     (general-define-key "a" #'a)
                     (after! helm
                       (general-define-key "b" #'b "c" #'c))))
          (expect '(map! (:after helm "b" #'b "c" #'c) "a" #'a)
                  :to-bind
                  '(progn
                     (after! helm
                       (general-define-key "b" #'b "c" #'c))
                     (general-define-key "a" #'a))))

        (it "nests `after!' blocks"
          (expect '(map! :after x "a" #'a
                         (:after y "b" #'b
                           (:after z "c" #'c)))
                  :to-bind
                  '(after! x
                     (progn
                       (general-define-key "a" #'a)
                       (after! y
                         (progn
                           (general-define-key "b" #'b)
                           (after! z
                             (general-define-key "c" #'c))))))))

        (it "nests `after!' blocks in other nested blocks"
          (expect '(map! :after x "a" #'a
                         (:when t "b" #'b
                           (:after z "c" #'c)))
                  :to-bind
                  '(after! x
                     (progn
                       (general-define-key "a" #'a)
                       (when t
                         (progn
                           (general-define-key "b" #'b)
                           (after! z (general-define-key "c" #'c)))))))))

      (describe ":desc"
        (it "add a :which-key property to a keybind's DEF"
          (expect '(map! :desc "A" "a" #'a)
                  :to-bind
                  `(general-define-key "a" (list :def #'a :which-key "A")))))

      (describe ":if/:when/:unless"
        (it "wraps keys in a conditional block"
          (dolist (prop '(:if :when :unless))
            (let ((prop-fn (intern (doom-keyword-name prop))))
              (expect `(map! ,prop t "a" #'a "b" #'b)
                      :to-bind
                      `(,prop-fn t (general-define-key "a" #'a "b" #'b)))
              (expect `(map! (,prop t "a" #'a "b" #'b))
                      :to-bind
                      `(,prop-fn t (general-define-key "a" #'a "b" #'b))))))

        (it "nests conditional blocks"
          (expect '(map! (:when t "a" #'a (:when t "b" #'b)))
                  :to-bind
                  '(when t
                     (progn (general-define-key "a" #'a)
                            (when t (general-define-key "b" #'b)))))))

      (describe ":leader"
        (it "uses leader definer"
          (expect '(map! :leader "a" #'a "b" #'b)
                  :to-bind
                  '(define-leader-key! "a" #'a "b" #'b)))

        (it "it persists for nested keys"
          (expect '(map! :leader "a" #'a ("b" #'b))
                  :to-bind
                  '(progn (define-leader-key! "a" #'a)
                          (define-leader-key! "b" #'b)))))

      (describe ":localleader"
        (it "uses localleader definer"
          (expect '(map! :localleader "a" #'a "b" #'b)
                  :to-bind
                  '(define-localleader-key! "a" #'a "b" #'b)))

        (it "it persists for nested keys"
          (expect '(map! :localleader "a" #'a ("b" #'b))
                  :to-bind
                  '(progn (define-localleader-key! "a" #'a)
                          (define-localleader-key! "b" #'b)))))

      (describe ":map/:keymap"
        (it "specifies a single keymap for keys"
          (expect '(map! :map emacs-lisp-mode-map "a" #'a)
                  :to-bind
                  '(general-define-key :keymaps '(emacs-lisp-mode-map) "a" #'a)))

        (it "specifies multiple keymap for keys"
          (expect '(map! :map (lisp-mode-map emacs-lisp-mode-map) "a" #'a)
                  :to-bind
                  '(general-define-key :keymaps '(lisp-mode-map emacs-lisp-mode-map) "a" #'a))))

      (describe ":mode"
        (it "appends -map to MODE"
          (expect '(map! :mode emacs-lisp-mode "a" #'a)
                  :to-bind
                  '(general-define-key :keymaps '(emacs-lisp-mode-map) "a" #'a))))

      (describe ":prefix"
        (it "specifies a prefix for all keys"
          (expect '(map! :prefix "a" "x" #'x "y" #'y "z" #'z)
                  :to-bind
                  '(general-define-key :prefix "a" "x" #'x "y" #'y "z" #'z)))

        (it "overwrites previous inline :prefix properties"
          (expect '(map! :prefix "a" "x" #'x "y" #'y :prefix "b" "z" #'z)
                  :to-bind
                  '(progn (general-define-key :prefix "a" "x" #'x "y" #'y)
                          (general-define-key :prefix "b" "z" #'z))))

        (it "accumulates keys when nested"
          (expect '(map! (:prefix "a" "x" #'x (:prefix "b" "x" #'x)))
                  :to-bind
                  `(progn (general-define-key :prefix "a" "x" #'x)
                          (general-define-key :prefix (general--concat t "a" "b")
                                              "x" #'x)))))

      (describe ":alt-prefix"
        (it "specifies a prefix for all keys"
          (expect '(map! :alt-prefix "a" "x" #'x "y" #'y "z" #'z)
                  :to-bind
                  '(general-define-key :non-normal-prefix "a" "x" #'x "y" #'y "z" #'z)))

        (it "overwrites previous inline :alt-prefix properties"
          (expect '(map! :alt-prefix "a" "x" #'x "y" #'y :alt-prefix "b" "z" #'z)
                  :to-bind
                  '(progn (general-define-key :non-normal-prefix "a" "x" #'x "y" #'y)
                          (general-define-key :non-normal-prefix "b" "z" #'z))))

        (it "accumulates keys when nested"
          (expect '(map! (:alt-prefix "a" "x" #'x (:alt-prefix "b" "x" #'x)))
                  :to-bind
                  `(progn (general-define-key :non-normal-prefix "a" "x" #'x)
                          (general-define-key :non-normal-prefix (general--concat t "a" "b")
                                              "x" #'x)))))

      (describe ":textobj"
        (it "defines keys in evil-{inner,outer}-text-objects-map"
          (expect '(map! :textobj "a" #'inner #'outer)
                  :to-bind
                  '(map! (:map evil-inner-text-objects-map "a" #'inner)
                         (:map evil-outer-text-objects-map "a" #'outer))))))))
