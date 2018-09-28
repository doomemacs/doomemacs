;; -*- no-byte-compile: t; -*-
;;; lang/web/test/test-autoload-web.el

(describe "lang/web"
  (describe "+html"
    (before-all (load! "../autoload/html.el"))

    (describe "encode entities"
      (it "encodes strings with html entities"
        (expect (+web-encode-entities "H€llø wørld")
                :to-equal "H&euro;ll&oslash; w&oslash;rld"))
      (it "does nothing when html entities are absent"
        (expect (+web-encode-entities "Hello world")
                :to-equal "Hello world")))

    (describe "decode entities"
      (it "decodes strings with html entities"
        (expect (+web-decode-entities "H&euro;ll&oslash; w&oslash;rld")
                :to-equal "H€llø wørld"))
      (it "does nothing when html entities are absent"
        (expect (+web-decode-entities "Hello world")
                :to-equal "Hello world"))))

  (describe "+css"
    :var (css-indent-offset)
    (before-all
      (load! "../autoload/css.el")
      (require 'smartparens)
      (smartparens-mode +1))
    (after-all
      (smartparens-mode -1)
      (unload-feature 'smartparens t))

    (before-each
      (setq css-indent-offset 2)
      (set-buffer (get-buffer-create "css"))
      (delay-mode-hooks (css-mode)))
    (after-each
      (kill-buffer (get-buffer "css")))

    (describe "toggle-inline-or-block"
      (after-each
        (quiet! (+css/toggle-inline-or-block))
        (expect (string-trim (buffer-string)) :to-equal
                (string-join
                 '("body {"
                   "  color: red;"
                   "  font-size: 2em;"
                   "}")
                 "\n")))

      (describe "css-mode"
        (it "converts inline statements into multiline blocks"
          (insert! "body { color: red{0}; font-size: 2em; }"))
        (it "works when cursor is on closing brace"
          (insert! "body { color: red; font-size: 2em; {0}}"))
        (it "works when cursor is on opening brace"
          (insert! "body {{0} color: red; font-size: 2em; }"))
        (it "works when cursor is on same line"
          (insert! "{0}body { color: red; font-size: 2em; }"))))

    (describe "comment-indent-new-line"
      (before-each
        (delay-mode-hooks (scss-mode)))

      (it "continues commented lines on newline"
        (insert! "// test{0}\n"
                 "body { color: red; font-size: 2em; }")
        (+css/comment-indent-new-line)
        (expect (string-trim (buffer-string)) :to-equal
                (string-join
                 '("// test"
                   "// "
                   "body { color: red; font-size: 2em; }")
                 "\n"))
        (expect (eolp))
        (expect (line-number-at-pos) :to-be 2))
      (it "preserves indentation within continued comments"
        (insert! "//       test{0}\n"
                 "body { color: red; font-size: 2em; }")
        (+css/comment-indent-new-line)
        (expect (string-trim (buffer-string)) :to-equal
                (string-join
                 '("//       test"
                   "//       "
                   "body { color: red; font-size: 2em; }")
                 "\n"))
        (expect (eolp))
        (expect (line-number-at-pos) :to-be 2)))))


;;
