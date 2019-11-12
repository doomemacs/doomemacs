;; -*- no-byte-compile: t; -*-
;;; lang/org/test/test-autoload-org.el

(describe "lang/org"
  ;; `+org/insert-item'
  (describe "insert-item"
    (before-all
      (require 'org)
      (load! "../autoload/org.el"))
    (after-all
      (unload-feature 'org t))

    (before-each
      (set-buffer (get-buffer-create "org"))
      (erase-buffer)
      (delay-mode-hooks (org-mode)))
    (after-each
      (kill-buffer (get-buffer "org")))

    (describe "headlines"
      (it "opens new headline below"
        (insert!! "* {0}Header")
        (+org/insert-item-below 1)
        (expect (eobp))
        (expect (buffer-substring-no-properties (point-min) (point-max))
                :to-equal "* Header\n* "))

      (it "opens new headline above"
        (insert!! "* {0}Header")
        (+org/insert-item-above 1)
        (expect (eolp))
        (expect (buffer-substring-no-properties (point-min) (point-max))
                :to-equal "* \n* Header"))

      (it "appends headlines, skipping subtrees"
        (insert!! "** {0}First\n"
                 "*** sub 1\n"
                 "*** sub 2\n"
                 "**** subsub 1\n"
                 "** Header")
        (+org/insert-item-below 1)
        (expect (eolp))
        (expect (line-number-at-pos) :to-be 5)
        (expect (buffer-substring-no-properties (point-min) (point-max))
                :to-equal
                (string-join '("** First"
                               "*** sub 1"
                               "*** sub 2"
                               "**** subsub 1"
                               "** "
                               "** Header")
                             "\n")))
      (it "prepends headlines, skipping subtrees"
        (insert!! "** First\n"
                 "*** sub 1\n"
                 "*** sub 2\n"
                 "**** {0}subsub 1\n"
                 "** Header")
        (+org/insert-item-above 1)
        (expect (eolp))
        (expect (line-number-at-pos) :to-be 4)
        (expect (buffer-substring-no-properties (point-min) (point-max))
                :to-equal
                (string-join '("** First"
                               "*** sub 1"
                               "*** sub 2"
                               "**** "
                               "**** subsub 1"
                               "** Header")
                             "\n"))))

    (describe "plain lists"
      (it "appends items"
        (insert!! "+ {0}List item")
        (+org/insert-item-below 1)
        (expect (buffer-substring-no-properties (point-min) (point-max))
                :to-equal "+ List item\n+ "))
      (it "prepends items"
        (insert!! "+ {0}List item")
        (+org/insert-item-above 1)
        (expect (buffer-substring-no-properties (point-min) (point-max))
                :to-equal "+ \n+ List item"))

      (it "appends items, but skips over child items"
        (insert!! "+ {0}List item\n"
                 "  + Sub item\n"
                 "+ List item")
        (+org/insert-item-below 1)
        (expect (buffer-substring-no-properties (point-min) (point-max))
                :to-equal
                (string-join '("+ List item"
                               "  + Sub item"
                               "+ "
                               "+ List item")
                             "\n")))
      (it "prepends items, but skips over child items"
        (insert!! "+ List item\n"
                 "  + Sub item\n"
                 "+ {0}List item")
        (+org/insert-item-above 1)
        (expect (buffer-substring-no-properties (point-min) (point-max))
                :to-equal
                (string-join '("+ List item"
                               "  + Sub item"
                               "+ "
                               "+ List item")
                             "\n"))))

    (describe "numbered lists"
      (it "appends items and updates numbers"
        (insert!! "1. {0}List item\n"
                 "2. Sub item\n"
                 "3. List item")
        (+org/insert-item-below 1)
        (expect (buffer-substring-no-properties (point-min) (point-max))
                :to-equal
                (string-join '("1. List item"
                               "2. "
                               "3. Sub item"
                               "4. List item")
                             "\n")))
      (it "prepends items and updates numbers"
        (insert!! "1. List item\n"
                 "2. Sub item\n"
                 "3. {0}List item")
        (+org/insert-item-above 1)
        (expect (buffer-substring-no-properties (point-min) (point-max))
                :to-equal
                (string-join '("1. List item"
                               "2. Sub item"
                               "3. "
                               "4. List item")
                             "\n"))))))
