;;; app/regex/config.el

;; Often, I find myself writing regular expressions that could terrify seasoned
;; programmers (or little children). To hone my regex fu, I need a regex
;; playground. Sure, there's regexr.com, but don't be silly, that's not Emacs.
;;
;; Sadly, the Emacs' regex syntax is niche and lacks support for a few
;; questionably useful features, like lookaround assertions, conditionals, case
;; modifiers or backreferences, among others. No, I want PCRE. I am going to
;; have my cake and eat it too, damn it!
;;
;; Workflow:
;;   + Invoke `=regex' (if opened with C-u, opens in separate workspace with a
;;     dummy text buffer).
;;   + A regex window will popup up. Any matches will be highlighted in the
;;     original buffer.
;;   + C-c C-k to close it
;;   + TODO C-c C-e to export to various langauges
;;
;; WARNING: THIS IS A WORK IN PROGRESS

(defvar +regex-workspace-name "*regex*"
  "TODO")

(defvar +regex-default-backend 'perl
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

(set-popup-rules!
 '(("^\\*doom-regex\\*$" :size 4 :quit nil)
   ("^\\*doom-regex-groups" :side 'left :size 28 :select nil :quit nil)))

