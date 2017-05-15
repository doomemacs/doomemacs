;;; app/regex/config.el

;; Provides a Regex IDE, invoked by `=regex'. If opened with C-u, opens in a
;; separate frame with a dummy text buffer.
;;
;; WARNING: THIS IS A WORK IN PROGRESS

(defvar +regex-workspace-name "*regex*"
  "TODO")

(defvar +regex-default-backend 'emacs
  "The backend used to process regular expressions.
The `emacs' backend handles regular expressions directly.
The `perl' backend talks to a perl subprocess to do the handling.")

(defvar +regex-dummy-text
  "Welcome to DOOM Emacs, proudly hosted by the demons of hell!

Edit the Expression & Text to see matches. Roll over matches or the expression
for details. Undo mistakes with ctrl-z. Save Favorites & Share expressions with
friends or the Community. Explore your results with Tools. A full Reference &
Help is available in the Library, or watch the video Tutorial.

Sample text for testing:
abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
0123456789 _+-.,!@#$%^&*();\/|<>\"'
12345 -98.7 3.141 .6180 9,000 +42
555.123.4567	+1-(800)-555-2468
foo@demo.net	bar.ba@test.co.uk
www.demo.com	http://foo.co.uk/
http://regexr.com/foo.html?q=bar
https://mediatemple.net"
  "TODO")

(set! :popup
  '("*doom-regex*" :size 4 :select t :noesc t :autokill t)
  '("*doom-regex-groups*" :align left :size 30 :noselect t :noesc t :autokill t))

