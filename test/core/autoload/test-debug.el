;;; test/core/autoload/test-debug.el

(def-test-group! core/autoload/debug
  (ert-deftest what-face ()
    (with-temp-buffer
      (insert (propertize "Hello " 'face 'font-lock-keyword-face))
      (insert "world")

      (should (equal (doom/what-face (point-min)) '((font-lock-keyword-face) ())))
      (should-not (doom/what-face (point-max)))))

  (ert-deftest what-face-overlays ()
    (with-temp-buffer
      (insert "Hello world")
      (let ((ov (make-overlay 1 6)))
        (overlay-put ov 'face 'font-lock-keyword-face))

      (should (equal (doom/what-face (point-min)) '(() (font-lock-keyword-face))))
      (should-not (doom/what-face (point-max))))))
