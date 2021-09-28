;; -*- no-byte-compile: t; -*-
;;; core/test/test-autoload-message.el

(describe "core/autoload/format"
  (describe "format!"
    :var (doom-output-backend)
    (before-all
      (setq doom-output-backend 'ansi))

    (it "should be a drop-in replacement for `format'"
      (expect (format! "Hello %s" "World")
              :to-equal "Hello World"))

    (it "supports ansi coloring in noninteractive sessions"
      (expect (format! (red "Hello %s") "World")
              :to-equal "[31mHello World[0m"))

    (it "supports text properties in interactive sessions"
      (let ((doom-output-backend 'text-properties))
        (expect (get-text-property 0 'face (format! (red "Hello %s") "World"))
                :to-equal (list :foreground (face-foreground 'term-color-red)))))

    (it "supports nested color specs"
      (expect (format! (bold (red "Hello %s")) "World")
              :to-equal (format "\e[%dm%s\e[0m" 1
                                (format "\e[%dm%s\e[0m" 31 "Hello World")))
      (expect (format! (on-red (bold "Hello %s")) "World")
              :to-equal (format "\e[%dm%s\e[0m" 41
                                (format "\e[%dm%s\e[0m" 1 "Hello World")))
      (expect (format! (dark (white "Hello %s")) "World")
              :to-equal (format "\e[%dm%s\e[0m" 2
                                (format "\e[%dm%s\e[0m" 37 "Hello World"))))

    (it "supports dynamic color apply syntax"
      (expect (format! (color 'red "Hello %s") "World")
              :to-equal (format! (red "Hello %s") "World"))
      (expect (format! (color (if nil 'red 'blue) "Hello %s") "World")
              :to-equal (format! (blue "Hello %s") "World"))))

  (xdescribe "insert!")
  (xdescribe "print!")
  (xdescribe "print-group!")
  (xdescribe "error!")
  (xdescribe "user-error!"))
