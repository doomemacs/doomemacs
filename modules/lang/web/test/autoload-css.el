;; -*- no-byte-compile: t; -*-
;;; lang/web/test/autoload-css.el

(def-test! toggle-inline-or-block
  (let ((css-indent-offset 2))
    (should-buffer!
        ("body { color: red{0}; font-size: 2em; }")
        ("body {"
         "  color: red{|};"
         "  font-size: 2em;"
         "}")
      (delay-mode-hooks (css-mode))
      (+css/toggle-inline-or-block))))

(def-test! comment-indent-new-line
  (should-buffer!
      ("// test{0}"
       "body { color: red; font-size: 2em; }")
      ("// test"
       "// {|}"
       "body { color: red; font-size: 2em; }")
    (delay-mode-hooks (scss-mode))
    (+css/comment-indent-new-line))
  (should-buffer!
      ("//       test{0}"
       "body { color: red; font-size: 2em; }")
      ("//       test"
       "//       {|}"
       "body { color: red; font-size: 2em; }")
    (delay-mode-hooks (scss-mode))
    (+css/comment-indent-new-line)))

