;; -*- no-byte-compile: t; -*-
;;; core/test/autoload-message.el

;; ansi messages
(def-test! ansi-format
  (let ((noninteractive t))
    (should (equal (format! "Hello %s" "World")
                   "Hello World"))
    (should (equal (format! (red "Hello %s" "World"))
                   "[31mHello World[0m"))
    (should (equal (format! (green "Hello %s" "World"))
                   (format "\e[%dm%s\e[0m"
                           (cdr (assq 'green doom-message-fg))
                           "Hello World")))
    (should (equal (format! (on-red "Hello %s" "World"))
                   (format "\e[%dm%s\e[0m"
                           (cdr (assq 'on-red doom-message-bg))
                           "Hello World")))
    (should (equal (format! (bold "Hello %s" "World"))
                   (format "\e[%dm%s\e[0m"
                           (cdr (assq 'bold doom-message-fx))
                           "Hello World")))))

(def-test! ansi-format-nested
  (let ((noninteractive t))
    (should (equal (format! (bold (red "Hello %s" "World")))
                   (format "\e[%dm%s\e[0m" 1
                           (format "\e[%dm%s\e[0m" 31 "Hello World"))))
    (should (equal (format! (on-red (bold "Hello %s" "World")))
                   (format "\e[%dm%s\e[0m" 41
                           (format "\e[%dm%s\e[0m" 1 "Hello World"))))
    (should (equal (format! (dark (white "Hello %s" "World")))
                   (format "\e[%dm%s\e[0m" 2
                           (format "\e[%dm%s\e[0m" 37 "Hello World"))))))

(def-test! ansi-format-apply
  (let ((noninteractive t))
    (should (equal (format! (color 'red "Hello %s" "World"))
                   (format! (red "Hello %s" "World"))))
    (should (equal (format! (color (if nil 'red 'blue) "Hello %s" "World"))
                   (format! (blue "Hello %s" "World"))))))
